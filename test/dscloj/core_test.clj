(ns dscloj.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [dscloj.core :as dscloj]))

(deftest module->prompt-test
  (testing "Basic prompt generation with inputs and outputs"
    (let [module {:inputs [{:name :question
                           :spec :string
                           :description "The question to answer"}]
                  :outputs [{:name :answer
                            :spec :string
                            :description "The answer"}]
                  :instructions "Answer questions accurately."}
          prompt (dscloj/module->prompt module)]
      (is (string? prompt))
      (is (re-find #"Your input fields are:" prompt))
      (is (re-find #"Your output fields are:" prompt))
      (is (re-find #"question" prompt))
      (is (re-find #"answer" prompt))
      (is (re-find #"Answer questions accurately\." prompt))))

  (testing "Prompt generation with instructions including rules"
    (let [module {:inputs [{:name :text :spec :string :description "Input text"}]
                  :outputs [{:name :result :spec :string :description "Result"}]
                  :instructions "Process text.\n\nSTRICT RULES:\n1. Be concise\n2. Be accurate"}
          prompt (dscloj/module->prompt module)]
      (is (re-find #"STRICT RULES:" prompt))
      (is (re-find #"Be concise" prompt))
      (is (re-find #"Process text\." prompt))))

  (testing "Prompt generation with boolean output type"
    (let [module {:inputs [{:name :statement :spec :string :description "A statement"}]
                  :outputs [{:name :is_true :spec :boolean :description "Is it true?"}]
                  :instructions "Verify statements."}
          prompt (dscloj/module->prompt module)]
      (is (re-find #"True or False" prompt))))

  (testing "Prompt generation without instructions"
    (let [module {:inputs [{:name :x :spec :string :description "Input"}]
                  :outputs [{:name :y :spec :string :description "Output"}]}
          prompt (dscloj/module->prompt module)]
      (is (string? prompt))
      (is (not (re-find #"instructions" prompt))))))

(deftest parse-output-test
  (testing "Parse string output"
    (let [module {:outputs [{:name :answer :spec :string}]}
          response "[[ ## answer ## ]]\nParis"
          result (dscloj/parse-output response module)]
      (is (= "Paris" (:answer result)))))

  (testing "Parse boolean output - True"
    (let [module {:outputs [{:name :is_valid :spec :boolean}]}
          response "[[ ## is_valid ## ]]\nTrue"
          result (dscloj/parse-output response module)]
      (is (true? (:is_valid result)))))

  (testing "Parse boolean output - true (lowercase)"
    (let [module {:outputs [{:name :is_valid :spec :boolean}]}
          response "[[ ## is_valid ## ]]\ntrue"
          result (dscloj/parse-output response module)]
      (is (true? (:is_valid result)))))

  (testing "Parse boolean output - False"
    (let [module {:outputs [{:name :is_valid :spec :boolean}]}
          response "[[ ## is_valid ## ]]\nFalse"
          result (dscloj/parse-output response module)]
      (is (false? (:is_valid result)))))

  (testing "Parse integer output"
    (let [module {:outputs [{:name :count :spec :int}]}
          response "[[ ## count ## ]]\n42"
          result (dscloj/parse-output response module)]
      (is (= 42 (:count result)))))

  (testing "Parse float output"
    (let [module {:outputs [{:name :score :spec :double}]}
          response "[[ ## score ## ]]\n0.95"
          result (dscloj/parse-output response module)]
      (is (= 0.95 (:score result)))))

  (testing "Parse multiple outputs"
    (let [module {:outputs [{:name :answer :spec :string}
                           {:name :confidence :spec :double}
                           {:name :is_confident :spec :boolean}]}
          response (str "[[ ## answer ## ]]\nParis\n"
                       "[[ ## confidence ## ]]\n0.98\n"
                       "[[ ## is_confident ## ]]\nTrue")
          result (dscloj/parse-output response module)]
      (is (= "Paris" (:answer result)))
      (is (= 0.98 (:confidence result)))
      (is (true? (:is_confident result)))))

  (testing "Parse output with extra whitespace"
    (let [module {:outputs [{:name :answer :spec :string}]}
          response "[[ ## answer ## ]]\n  Paris  \n"
          result (dscloj/parse-output response module)]
      (is (= "Paris" (:answer result)))))

  (testing "Parse output with multiline content"
    (let [module {:outputs [{:name :explanation :spec :string}]}
          response "[[ ## explanation ## ]]\nLine 1\nLine 2\nLine 3"
          result (dscloj/parse-output response module)]
      (is (= "Line 1\nLine 2\nLine 3" (:explanation result)))))

  (testing "Invalid integer returns original string"
    (let [module {:outputs [{:name :count :spec :int}]}
          response "[[ ## count ## ]]\nnot-a-number"
          result (dscloj/parse-output response module)]
      (is (= "not-a-number" (:count result)))))

  (testing "Invalid float returns original string"
    (let [module {:outputs [{:name :score :spec :double}]}
          response "[[ ## score ## ]]\nnot-a-number"
          result (dscloj/parse-output response module)]
      (is (= "not-a-number" (:score result))))))

(deftest field-extraction-test
  (testing "Field name with underscores"
    (let [module {:outputs [{:name :my_field :spec :string}]}
          response "[[ ## my_field ## ]]\nValue"
          result (dscloj/parse-output response module)]
      (is (= "Value" (:my_field result)))))

  (testing "Field name with hyphens (keyword style)"
    (let [module {:outputs [{:name :my-field :spec :string}]}
          response "[[ ## my-field ## ]]\nValue"
          result (dscloj/parse-output response module)]
      (is (= "Value" (:my-field result))))))

;; =============================================================================
;; Malli Spec Tests
;; =============================================================================

(deftest spec->type-str-test
  (testing "Convert Malli specs to string types"
    (is (= "str" (dscloj/spec->type-str :string)))
    (is (= "int" (dscloj/spec->type-str :int)))
    (is (= "float" (dscloj/spec->type-str :double)))
    (is (= "float" (dscloj/spec->type-str :float)))
    (is (= "bool" (dscloj/spec->type-str :boolean))))
  
  (testing "Convert Malli predicate symbols to string types"
    (is (= "str" (dscloj/spec->type-str 'string?)))
    (is (= "int" (dscloj/spec->type-str 'int?)))
    (is (= "float" (dscloj/spec->type-str 'double?)))
    (is (= "bool" (dscloj/spec->type-str 'boolean?))))
  
  (testing "Convert vector specs"
    (is (= "str" (dscloj/spec->type-str [:string {:description "A string"}]))))
  
  (testing "Fallback for unknown types"
    (is (= "str" (dscloj/spec->type-str :unknown)))))

(deftest validation-test
  (testing "Valid field passes validation"
    (let [field {:name :age :spec :int :description "Age"}
          value 25]
      (is (= value (dscloj/validate-field field value)))))
  
  (testing "Invalid field throws exception"
    (let [field {:name :age :spec :int :description "Age"}
          value "not-a-number"]
      (is (thrown? Exception (dscloj/validate-field field value)))))
  
  (testing "Valid inputs pass validation"
    (let [fields [{:name :question :spec :string}
                  {:name :age :spec :int}]
          input {:question "What is 2+2?" :age 25}]
      (is (= input (dscloj/validate-inputs fields input)))))
  
  (testing "Invalid input throws exception"
    (let [fields [{:name :age :spec :int}]
          input {:age "not-a-number"}]
      (is (thrown? Exception (dscloj/validate-inputs fields input))))))

(deftest module-with-spec-test
  (testing "Module with Malli specs generates correct prompt"
    (let [module {:inputs [{:name :question
                           :spec [:string {:description "The question"}]}]
                  :outputs [{:name :answer
                            :spec [:string {:description "The answer"}]}]
                  :instructions "Answer accurately."}
          prompt (dscloj/module->prompt module)]
      (is (string? prompt))
      (is (re-find #"Your input fields are:" prompt))
      (is (re-find #"Your output fields are:" prompt))
      (is (re-find #"question" prompt))
      (is (re-find #"answer" prompt))
      (is (re-find #"Answer accurately\." prompt))))
  
  (testing "Module with multiple field types"
    (let [module {:inputs [{:name :text :spec :string :description "Input text"}
                          {:name :count :spec :int :description "Number of items"}]
                  :outputs [{:name :result :spec :string :description "Result"}
                           {:name :is_valid :spec :boolean :description "Is valid?"}
                           {:name :score :spec :double :description "Score"}]}
          prompt (dscloj/module->prompt module)]
      ;; Check that fields are present in the prompt
      (is (re-find #"text" prompt))
      (is (re-find #"count" prompt))
      (is (re-find #"result" prompt))
      (is (re-find #"is_valid" prompt))
      (is (re-find #"score" prompt))
      ;; Check that types are correctly converted
      (is (re-find #"str" prompt))
      (is (re-find #"int" prompt))
      (is (re-find #"bool" prompt))
      (is (re-find #"float" prompt)))))

;; =============================================================================
;; Complex Schema Tests
;; =============================================================================

(deftest complex-spec-test
  (testing "complex-spec? detects complex types"
    (is (dscloj/complex-spec? [:map [:x :string]]))
    (is (dscloj/complex-spec? [:vector :int]))
    (is (dscloj/complex-spec? [:sequential :string]))
    (is (dscloj/complex-spec? [:maybe :string]))
    (is (not (dscloj/complex-spec? [:enum "a" "b" "c"]))) ;; enums are plain values, not JSON
    (is (not (dscloj/complex-spec? :string)))
    (is (not (dscloj/complex-spec? :int)))
    (is (not (dscloj/complex-spec? [:string {:min 1}]))))

  (testing "spec->type-str handles map schemas"
    (is (= "json {one: str, two: str}"
           (dscloj/spec->type-str [:map [:one :string] [:two :string]])))
    (is (= "json {name: str, age: int}"
           (dscloj/spec->type-str [:map [:name :string] [:age :int]]))))

  (testing "spec->type-str handles nested maps"
    (is (= "json {user: json {name: str}}"
           (dscloj/spec->type-str [:map [:user [:map [:name :string]]]]))))

  (testing "spec->type-str handles optional fields"
    (is (= "json {name: str, email?: str}"
           (dscloj/spec->type-str [:map [:name :string] [:email {:optional true} :string]]))))

  (testing "spec->type-str handles vector schemas"
    (is (= "json array of str"
           (dscloj/spec->type-str [:vector :string])))
    (is (= "json array of int"
           (dscloj/spec->type-str [:vector :int]))))

  (testing "spec->type-str handles enum schemas"
    (is (= "one of: a, b, c"
           (dscloj/spec->type-str [:enum "a" "b" "c"]))))

  (testing "spec->type-str handles maybe schemas"
    (is (= "str or null"
           (dscloj/spec->type-str [:maybe :string])))))

(deftest parse-json-output-test
  (testing "Parse JSON object output"
    (let [module {:outputs [{:name :data
                             :spec [:map [:name :string] [:age :int]]}]}
          response "[[ ## data ## ]]\n{\"name\": \"John\", \"age\": 30}"
          result (dscloj/parse-output response module)]
      (is (= {:name "John" :age 30} (:data result)))))

  (testing "Parse JSON array output"
    (let [module {:outputs [{:name :items :spec [:vector :string]}]}
          response "[[ ## items ## ]]\n[\"a\", \"b\", \"c\"]"
          result (dscloj/parse-output response module)]
      (is (= ["a" "b" "c"] (:items result)))))

  (testing "Parse nested JSON output"
    (let [module {:outputs [{:name :user
                             :spec [:map [:profile [:map [:name :string]]]]}]}
          response "[[ ## user ## ]]\n{\"profile\": {\"name\": \"Alice\"}}"
          result (dscloj/parse-output response module)]
      (is (= {:profile {:name "Alice"}} (:user result)))))

  (testing "Parse enum as plain string"
    (let [module {:outputs [{:name :choice :spec [:enum "a" "b" "c"]}]}
          response "[[ ## choice ## ]]\nb"
          result (dscloj/parse-output response module)]
      (is (= "b" (:choice result)))))

  (testing "Invalid JSON falls back to string"
    (let [module {:outputs [{:name :data :spec [:map [:x :string]]}]}
          response "[[ ## data ## ]]\nnot valid json"
          result (dscloj/parse-output response module)]
      (is (= "not valid json" (:data result)))))

  (testing "Mixed complex and simple outputs"
    (let [module {:outputs [{:name :items :spec [:vector :string]}
                           {:name :count :spec :int}
                           {:name :valid :spec :boolean}]}
          response (str "[[ ## items ## ]]\n[\"x\", \"y\"]\n"
                       "[[ ## count ## ]]\n2\n"
                       "[[ ## valid ## ]]\nTrue")
          result (dscloj/parse-output response module)]
      (is (= ["x" "y"] (:items result)))
      (is (= 2 (:count result)))
      (is (true? (:valid result))))))

(deftest module-prompt-json-hint-test
  (testing "Prompt includes JSON hint for map outputs"
    (let [module {:outputs [{:name :data
                             :spec [:map [:x :string]]
                             :description "Data object"}]}
          prompt (dscloj/module->prompt module)]
      (is (re-find #"respond with valid JSON" prompt))))

  (testing "Prompt includes JSON hint for vector outputs"
    (let [module {:outputs [{:name :items
                             :spec [:vector :string]
                             :description "List of items"}]}
          prompt (dscloj/module->prompt module)]
      (is (re-find #"respond with valid JSON" prompt))))

  (testing "Prompt does not include JSON hint for simple outputs"
    (let [module {:outputs [{:name :answer
                             :spec :string
                             :description "The answer"}]}
          prompt (dscloj/module->prompt module)]
      (is (not (re-find #"respond with valid JSON" prompt))))))

;; =============================================================================
;; JSON Schema Conversion Tests (for function calling)
;; =============================================================================

(deftest malli-spec->json-schema-test
  (testing "Primitive types convert to JSON Schema"
    (is (= {:type "string"} (dscloj/malli-spec->json-schema :string)))
    (is (= {:type "integer"} (dscloj/malli-spec->json-schema :int)))
    (is (= {:type "number"} (dscloj/malli-spec->json-schema :double)))
    (is (= {:type "number"} (dscloj/malli-spec->json-schema :float)))
    (is (= {:type "boolean"} (dscloj/malli-spec->json-schema :boolean)))
    (is (= {} (dscloj/malli-spec->json-schema :any))))

  (testing "Predicate symbols convert to JSON Schema"
    (is (= {:type "string"} (dscloj/malli-spec->json-schema 'string?)))
    (is (= {:type "integer"} (dscloj/malli-spec->json-schema 'int?)))
    (is (= {:type "number"} (dscloj/malli-spec->json-schema 'double?)))
    (is (= {:type "boolean"} (dscloj/malli-spec->json-schema 'boolean?))))

  (testing "Enum converts to JSON Schema with allowed values"
    (is (= {:type "string" :enum ["a" "b" "c"]}
           (dscloj/malli-spec->json-schema [:enum "a" "b" "c"])))
    (is (= {:type "string" :enum ["unchanged" "changed"]}
           (dscloj/malli-spec->json-schema [:enum :unchanged :changed])))
    (is (= {:type "string" :enum ["decision/unchanged" "decision/changed"]}
           (dscloj/malli-spec->json-schema
            [:enum :decision/unchanged :decision/changed])))
    (is (= {:type "string" :enum [":unchanged" "changed"]}
           (dscloj/malli-spec->json-schema [:enum ":unchanged" "changed"]))))

  (testing "Literal values convert to JSON Schema constants"
    (is (= {:const "invoke"}
           (dscloj/malli-spec->json-schema [:= :invoke])))
    (is (= {:const "decision/invoke"}
           (dscloj/malli-spec->json-schema [:= :decision/invoke])))
    (is (= {:const ":invoke"}
           (dscloj/malli-spec->json-schema [:= ":invoke"])))
    (is (= {:const false}
           (dscloj/malli-spec->json-schema [:= false]))))

  (testing "Or converts every structured alternative"
    (is (= {:oneOf
            [{:type "object"
              :properties {"action" {:const "invoke"}
                           "capability" {:type "string"}}
              :required ["action" "capability"]}
             {:type "object"
              :properties {"action" {:const "respond"}
                           "message" {:type "string"}}
              :required ["action" "message"]}]}
           (dscloj/malli-spec->json-schema
            [:or
             [:map [:action [:= :invoke]] [:capability :string]]
             [:map [:action [:= :respond]] [:message :string]]]))))

  (testing "Maybe converts to nullable"
    (is (= {:type "string" :nullable true}
           (dscloj/malli-spec->json-schema [:maybe :string])))
    (testing "without changing the child type when schema properties are present"
      (is (= {:type "integer" :nullable true}
             (dscloj/malli-spec->json-schema
              [:maybe {:description "ACT composite score"} :int])))))

  (testing "Map converts to object with properties"
    (let [schema (dscloj/malli-spec->json-schema [:map [:name :string] [:age :int]])]
      (is (= "object" (:type schema)))
      (is (= {:type "string"} (get-in schema [:properties "name"])))
      (is (= {:type "integer"} (get-in schema [:properties "age"])))
      (is (= ["name" "age"] (:required schema)))))

  (testing "Map with optional fields"
    (let [schema (dscloj/malli-spec->json-schema [:map
                                                   [:name :string]
                                                   [:email {:optional true} :string]])]
      (is (= ["name"] (:required schema)))
      (is (some? (get-in schema [:properties "email"])))))

  (testing "Vector converts to array"
    (is (= {:type "array" :items {:type "string"}}
           (dscloj/malli-spec->json-schema [:vector :string])))
    (is (= {:type "array" :items {:type "integer"}}
           (dscloj/malli-spec->json-schema [:vector :int])))
    (is (= {:type "array" :items {:type "integer"}}
           (dscloj/malli-spec->json-schema
            [:vector {:description "Scores"} :int]))))

  (testing "Sequential converts to array"
    (is (= {:type "array" :items {:type "string"}}
           (dscloj/malli-spec->json-schema [:sequential :string])))
    (is (= {:type "array" :items {:type "integer"}}
           (dscloj/malli-spec->json-schema
            [:sequential {:description "Scores"} :int]))))

  (testing "Set converts to array with uniqueItems"
    (is (= {:type "array" :items {:type "string"} :uniqueItems true}
           (dscloj/malli-spec->json-schema [:set :string])))
    (is (= {:type "array" :items {:type "string"} :uniqueItems true}
           (dscloj/malli-spec->json-schema
            [:set {:description "Labels"} :string]))))

  (testing "Map-of converts to object with additionalProperties"
    (is (= {:type "object" :additionalProperties {:type "string"}}
           (dscloj/malli-spec->json-schema [:map-of :keyword :string])))
    (is (= {:type "object" :additionalProperties {:type "integer"}}
           (dscloj/malli-spec->json-schema
            [:map-of {:description "Scores by subject"} :keyword :int]))))

  (testing "Tuple converts every positional child when properties are present"
    (is (= {:type "array"
            :items [{:type "string"} {:type "integer"}]}
           (dscloj/malli-spec->json-schema
            [:tuple {:description "Label and score"} :string :int]))))

  (testing "Nested structures"
    (let [schema (dscloj/malli-spec->json-schema
                   [:map [:user [:map [:name :string]]]])]
      (is (= "object" (:type schema)))
      (is (= "object" (get-in schema [:properties "user" :type])))
      (is (= {:type "string"} (get-in schema [:properties "user" :properties "name"])))))

  (testing "Wrapped specs unwrap correctly"
    (is (= {:type "string"}
           (dscloj/malli-spec->json-schema [:string {:min 1}])))))

(deftest outputs->tool-definition-test
  (testing "Creates valid tool definition from simple output"
    (let [module {:outputs [{:name :answer :spec :string :description "The answer"}]
                  :instructions "Answer the question"}
          tool-def (dscloj/outputs->tool-definition module)]
      (is (= "function" (:type tool-def)))
      (is (= "submit_response" (get-in tool-def [:function :name])))
      (is (= "Answer the question" (get-in tool-def [:function :description])))
      (is (= "object" (get-in tool-def [:function :parameters :type])))
      (is (= {:type "string" :description "The answer"}
             (get-in tool-def [:function :parameters :properties "answer"])))
      (is (= ["answer"] (get-in tool-def [:function :parameters :required])))))

  (testing "Creates tool definition with multiple outputs"
    (let [module {:outputs [{:name :score :spec :double :description "Score 0-1"}
                            {:name :valid :spec :boolean :description "Is valid"}]
                  :instructions "Evaluate"}
          tool-def (dscloj/outputs->tool-definition module)]
      (is (= {:type "number" :description "Score 0-1"}
             (get-in tool-def [:function :parameters :properties "score"])))
      (is (= {:type "boolean" :description "Is valid"}
             (get-in tool-def [:function :parameters :properties "valid"])))
      (is (= ["score" "valid"] (get-in tool-def [:function :parameters :required])))))

  (testing "Creates tool definition with complex output"
    (let [module {:outputs [{:name :data
                             :spec [:map [:items [:vector :string]] [:count :int]]
                             :description "Result data"}]}
          tool-def (dscloj/outputs->tool-definition module)]
      (is (= "object" (get-in tool-def [:function :parameters :properties "data" :type])))
      (is (= {:type "array" :items {:type "string"}}
             (get-in tool-def [:function :parameters :properties "data" :properties "items"])))))

  (testing "Uses canonical JSON spellings for keyword enum outputs"
    (let [module {:outputs [{:name :outcome
                             :spec [:enum :unchanged :changed]
                             :description "Learning outcome"}]}
          tool-def (dscloj/outputs->tool-definition module)]
      (is (= {:type "string"
              :enum ["unchanged" "changed"]
              :description "Learning outcome"}
             (get-in tool-def
                     [:function :parameters :properties "outcome"])))))

  (testing "Keeps a root union of maps structured in the tool definition"
    (let [module {:outputs
                  [{:name :decision
                    :spec [:or
                           [:map
                            [:action [:= :invoke]]
                            [:capability :string]
                            [:world-changing? :boolean]]
                           [:map
                            [:action [:= :respond]]
                            [:message :string]]]}]}
          decision-schema
          (get-in (dscloj/outputs->tool-definition module)
                  [:function :parameters :properties "decision"])]
      (is (nil? (:type decision-schema)))
      (is (= 2 (count (:oneOf decision-schema))))
      (is (every? #(= "object" (:type %)) (:oneOf decision-schema)))
      (is (= {:const "invoke"}
             (get-in decision-schema
                     [:oneOf 0 :properties "action"])))))

  (testing "Uses default description when instructions not provided"
    (let [module {:outputs [{:name :x :spec :string}]}
          tool-def (dscloj/outputs->tool-definition module)]
      (is (= "Submit the structured response"
             (get-in tool-def [:function :description]))))))

(deftest function-calling-parses-structured-output-values-test
  (let [parse-tool-call-response
        (ns-resolve 'dscloj.core 'parse-tool-call-response)
        response
        {:choices
         [{:message
           {:tool-calls
            [{:function
              {:arguments
               "{\"decision\":{\"action\":\"invoke\",\"capability\":\"filesystem.read-text\",\"world-changing?\":false}}"}}]}}]}
        parsed
        (parse-tool-call-response
         response
         [{:name :decision
           :spec [:or
                  [:map
                   [:action [:= :invoke]]
                   [:capability :string]
                   [:world-changing? :boolean]]
                  [:map [:action [:= :respond]] [:message :string]]]}])]
    (is (= {:decision
            {:action "invoke"
             :capability "filesystem.read-text"
             :world-changing? false}}
           parsed))
    (is (map? (:decision parsed))
        "the structured value is not returned as JSON encoded text")))

;; =============================================================================
;; Multimodal Image Input Tests
;; =============================================================================

(deftest build-message-content-test
  (testing "Text-only module returns plain string"
    (let [module {:inputs [{:name :question :spec :string :description "Q"}]
                  :outputs [{:name :answer :spec :string :description "A"}]}
          result (dscloj/build-message-content module "Hello" {:question "Hi"})]
      (is (string? result))
      (is (= "Hello" result))))

  (testing "Module with image input returns vector of content parts"
    (let [module {:inputs [{:name :question :spec :string :description "Q"}
                           {:name :photo :type :image :description "An image"}]
                  :outputs [{:name :answer :spec :string :description "A"}]}
          result (dscloj/build-message-content module "Describe this"
                   {:question "What is this?" :photo "data:image/png;base64,abc123"})]
      (is (vector? result))
      (is (= 2 (count result)))
      (is (= {:type "text" :text "Describe this"} (first result)))
      (is (= {:type "image_url" :image_url {:url "data:image/png;base64,abc123"}}
             (second result)))))

  (testing "Single image value (not wrapped in seq) works"
    (let [module {:inputs [{:name :img :type :image :description "Image"}]
                  :outputs [{:name :out :spec :string}]}
          result (dscloj/build-message-content module "prompt" {:img "https://example.com/img.png"})]
      (is (vector? result))
      (is (= 2 (count result)))
      (is (= "https://example.com/img.png"
             (get-in (second result) [:image_url :url])))))

  (testing "Multiple images from a lazy seq work"
    (let [module {:inputs [{:name :photos :type :image :description "Photos"}]
                  :outputs [{:name :out :spec :string}]}
          urls (map #(str "https://example.com/" % ".png") (range 3))
          result (dscloj/build-message-content module "prompt" {:photos urls})]
      (is (vector? result))
      ;; 1 text part + 3 image parts
      (is (= 4 (count result)))
      (is (= "text" (:type (first result))))
      (is (every? #(= "image_url" (:type %)) (rest result)))))

  (testing "Nil image value is excluded"
    (let [module {:inputs [{:name :img :type :image :description "Image"}]
                  :outputs [{:name :out :spec :string}]}
          result (dscloj/build-message-content module "prompt" {:img nil})]
      (is (vector? result))
      (is (= 1 (count result)))
      (is (= {:type "text" :text "prompt"} (first result))))))

(deftest validate-inputs-skips-image-fields-test
  (testing "Image fields are skipped during validation"
    (let [fields [{:name :question :spec :string :description "Q"}
                  {:name :photo :type :image :spec :string :description "Photo"}]
          ;; photo value is not a string — would fail :string validation if not skipped
          input {:question "What is this?" :photo ["data:image/png;base64,abc"]}]
      (is (= input (dscloj/validate-inputs fields input)))))

  (testing "Non-image fields still validate"
    (let [fields [{:name :count :spec :int :description "Count"}
                  {:name :photo :type :image :description "Photo"}]
          input {:count "not-a-number" :photo "data:image/png;base64,abc"}]
      (is (thrown? Exception (dscloj/validate-inputs fields input))))))

(deftest module-prompt-excludes-image-inputs-test
  (testing "Image inputs are excluded from prompt template"
    (let [module {:inputs [{:name :question :spec :string :description "The question"}
                           {:name :photo :type :image :description "A photo"}]
                  :outputs [{:name :answer :spec :string :description "The answer"}]
                  :instructions "Describe the image."}
          prompt (dscloj/module->prompt module)]
      ;; question should appear in the prompt
      (is (re-find #"question" prompt))
      ;; photo should NOT appear in field listing or interaction format
      (is (not (re-find #"photo" prompt))))))
