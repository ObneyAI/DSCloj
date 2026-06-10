(ns dscloj.predict-stream-v2-test
  "Pure unit tests for predict-stream-v2 — no network. litellm.router/completion
  is redefined to return a hand-built core.async channel of fake chunks."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.async :as async]
            [litellm.router :as router]
            [litellm.streaming :as streaming]
            [dscloj.core :as dscloj]))

;; =============================================================================
;; Fixtures & helpers
;; =============================================================================

(def qa-module
  {:inputs [{:name :question :spec :string :description "The question to answer"}]
   :outputs [{:name :answer :spec :string :description "The answer"}]
   :instructions "Answer questions accurately."})

(defn- content-chunk
  "Build a fake streaming chunk carrying a content delta, optionally merged
  with extra top-level keys (e.g. :usage, :model)."
  ([text] (content-chunk text nil))
  ([text extra]
   (merge {:choices [{:delta {:content text}}]} extra)))

(defn- fake-stream
  "Return a closed-when-drained channel pre-loaded with chunks."
  [chunks]
  (let [ch (async/chan (max 1 (count chunks)))]
    (async/onto-chan! ch chunks)
    ch))

(defn- drain!
  "Blocking-take all events from ch until it closes.
  Returns [events closed?] — closed? is false if the 2s safety timeout fired."
  [ch]
  (loop [acc []]
    (let [[v port] (async/alts!! [ch (async/timeout 2000)])]
      (cond
        (and (= port ch) (nil? v)) [acc true]
        (= port ch) (recur (conj acc v))
        :else [acc false]))))

(defn- events-of [events event-type]
  (filterv #(= event-type (:dscloj/event %)) events))

(defmacro with-fake-stream
  "Redefine router/completion to capture the request in req-atom and return a
  fake stream of chunks, then run body."
  [req-atom chunks & body]
  `(with-redefs [router/completion (fn [_provider-config# request#]
                                     (reset! ~req-atom request#)
                                     (fake-stream ~chunks))]
     ~@body))

(def answer-chunks
  [(content-chunk "[[ ## answer ## ]]\n")
   (content-chunk "Par")
   (content-chunk "is")])

;; =============================================================================
;; Tests
;; =============================================================================

(deftest deltas-arrive-in-order-test
  (testing "(a) every raw content delta is emitted un-debounced, in order"
    (let [req (atom nil)]
      (with-fake-stream req answer-chunks
        (let [ch (dscloj/predict-stream-v2 :fake qa-module {:question "Capital of France?"}
                                           {:debounce-ms 0})
              [events closed?] (drain! ch)]
          (is closed?)
          (is (= ["[[ ## answer ## ]]\n" "Par" "is"]
                 (mapv :text (events-of events :delta)))))))))

(deftest fields-progressive-parse-test
  (testing "(b) :fields events reflect progressive parsing of marker output"
    (let [req (atom nil)]
      (with-fake-stream req answer-chunks
        (let [ch (dscloj/predict-stream-v2 :fake qa-module {:question "Capital of France?"}
                                           {:debounce-ms 0})
              [events closed?] (drain! ch)
              fields-events (events-of events :fields)]
          (is closed?)
          (is (seq fields-events) "at least one :fields event is emitted")
          (is (some #(= {:answer "Par"} (:fields %)) fields-events)
              "an intermediate partial parse is observable")
          (is (= {:answer "Paris"} (:fields (last fields-events)))
              "the last :fields event reflects the full accumulated parse"))))))

(deftest final-carries-outputs-usage-and-model-test
  (testing "(c) :final carries parsed outputs, accumulated usage, and model"
    (let [req (atom nil)
          chunks (conj answer-chunks
                       ;; trailing metadata chunk with no content delta
                       {:choices [{:delta {}}]
                        :usage {:prompt-tokens 10 :completion-tokens 20 :total-tokens 30}
                        :model "test-model"})]
      (with-fake-stream req chunks
        (let [ch (dscloj/predict-stream-v2 :fake qa-module {:question "Capital of France?"}
                                           {:debounce-ms 0})
              [events closed?] (drain! ch)
              finals (events-of events :final)
              final (last events)]
          (is closed?)
          (is (= 1 (count finals)) "exactly one :final event")
          (is (= :final (:dscloj/event final)) ":final is the last event")
          (is (= {:answer "Paris"} (:outputs final)))
          (is (= {:prompt-tokens 10 :completion-tokens 20 :total-tokens 30}
                 (:usage final)))
          (is (= "test-model" (:model final))))))))

(deftest usage-accumulates-across-chunks-test
  (testing "(c-bis) usage across chunks keeps the latest non-nil value per key
            (cumulative semantics — never summed)"
    (let [req (atom nil)
          chunks [(content-chunk "[[ ## answer ## ]]\nParis"
                                 {:usage {:prompt-tokens 10 :completion-tokens 20 :total-tokens 30}
                                  :model "model-1"})
                  {:choices [{:delta {}}]
                   :usage {:prompt-tokens 5 :completion-tokens 7 :total-tokens 12}
                   :model "model-2"}]]
      (with-fake-stream req chunks
        (let [ch (dscloj/predict-stream-v2 :fake qa-module {:question "Capital of France?"}
                                           {:debounce-ms 0})
              [events closed?] (drain! ch)
              final (last events)]
          (is closed?)
          (is (= {:prompt-tokens 5 :completion-tokens 7 :total-tokens 12}
                 (:usage final))
              "latest non-nil per key wins; total = (+ prompt completion)")
          (is (= "model-2" (:model final))
              ":model comes from the last chunk carrying it"))))))

(deftest anthropic-cumulative-usage-test
  (testing "(c-ter) Anthropic-style streaming usage: message_start carries prompt
            tokens, message_delta carries CUMULATIVE completion tokens with a
            nil :prompt-tokens — latest non-nil per key, total recomputed"
    (let [req (atom nil)
          chunks [;; message_start-style chunk: prompt known, ~1 completion token
                  (content-chunk "[[ ## answer ## ]]\n"
                                 {:usage {:prompt-tokens 25 :completion-tokens 1 :total-tokens 26}
                                  :model "claude-model"})
                  (content-chunk "Par")
                  ;; message_delta-style chunk: nil prompt, cumulative completion
                  (content-chunk "is"
                                 {:usage {:prompt-tokens nil :completion-tokens 18 :total-tokens 18}})]]
      (with-fake-stream req chunks
        (let [ch (dscloj/predict-stream-v2 :fake qa-module {:question "Capital of France?"}
                                           {:debounce-ms 0})
              [events closed?] (drain! ch)
              final (last events)]
          (is closed?)
          (is (= :final (:dscloj/event final)))
          (is (= {:answer "Paris"} (:outputs final)))
          (is (= {:prompt-tokens 25 :completion-tokens 18 :total-tokens 43}
                 (:usage final))
              "nil prompt does not clobber or NPE; completion is cumulative not summed")
          (is (= "claude-model" (:model final))))))))

(deftest final-without-usage-test
  (testing "(d) when no chunk carries usage, :final has nil usage (and nil model)"
    (let [req (atom nil)]
      (with-fake-stream req answer-chunks
        (let [ch (dscloj/predict-stream-v2 :fake qa-module {:question "Capital of France?"}
                                           {:debounce-ms 0})
              [events closed?] (drain! ch)
              final (last events)]
          (is closed?)
          (is (= :final (:dscloj/event final)))
          (is (= {:answer "Paris"} (:outputs final)))
          (is (nil? (:usage final)))
          (is (nil? (:model final))))))))

(deftest error-chunk-terminates-stream-test
  (testing "(e) a litellm error chunk emits :error and closes with no :final"
    (let [req (atom nil)
          chunks [(content-chunk "[[ ## answer ## ]]\nPar")
                  (streaming/stream-error :openai "boom"
                                          :status 500
                                          :code "server_error")]]
      (with-fake-stream req chunks
        (let [ch (dscloj/predict-stream-v2 :fake qa-module {:question "Capital of France?"}
                                           {:debounce-ms 0})
              [events closed?] (drain! ch)
              error-events (events-of events :error)]
          (is closed?)
          (is (= 1 (count error-events)) "exactly one :error event")
          (is (= :error (:dscloj/event (last events))) ":error is the terminal event")
          (is (empty? (events-of events :final)) "no :final after :error")
          (let [err (:error (first error-events))]
            (is (= "boom" (:message err)))
            (is (= :openai (:provider err)))
            (is (= 500 (:http-status err)))))))))

(deftest channel-closes-after-terminal-event-test
  (testing "(f) the channel closes after the terminal event"
    (testing "after :final"
      (let [req (atom nil)]
        (with-fake-stream req answer-chunks
          (let [ch (dscloj/predict-stream-v2 :fake qa-module {:question "Q"}
                                             {:debounce-ms 0})
                [events closed?] (drain! ch)]
            (is closed?)
            (is (= :final (:dscloj/event (last events))))
            ;; subsequent takes return nil immediately
            (is (nil? (async/<!! ch)))))))
    (testing "after :error"
      (let [req (atom nil)]
        (with-fake-stream req [(streaming/stream-error :openai "down")]
          (let [ch (dscloj/predict-stream-v2 :fake qa-module {:question "Q"}
                                             {:debounce-ms 0})
                [events closed?] (drain! ch)]
            (is closed?)
            (is (= [:error] (mapv :dscloj/event events)))
            (is (nil? (async/<!! ch)))))))))

(deftest options-passthrough-test
  (testing "options are forwarded to the completion call, always with :stream true"
    (let [req (atom nil)]
      (with-fake-stream req answer-chunks
        (let [ch (dscloj/predict-stream-v2 :fake qa-module {:question "Q"}
                                           {:temperature 0.3
                                            :max-tokens 128
                                            :debounce-ms 0
                                            :validate? false})
              [_ closed?] (drain! ch)]
          (is closed?)
          (is (true? (:stream @req)))
          (is (= 0.3 (:temperature @req)))
          (is (= 128 (:max-tokens @req)))
          (is (not (contains? @req :debounce-ms)))
          (is (not (contains? @req :validate?)))
          (is (vector? (:messages @req))))))))
