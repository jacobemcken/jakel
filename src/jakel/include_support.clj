(ns jakel.include-support
  "Namespace for preprocessing Jekyll files to support the `include` tag
   which is deprecated from Liquid.

   https://jekyllrb.com/docs/includes/"
  (:require [clojure.string :as str]))

(defn wrap-with-str
  [s wrap-s]
  (str wrap-s s wrap-s))

(def include-syntax-pattern
  #"\{%\s*include\s*([^%]*)%\s*\}")

(defn include->render
  "Takes a match of `include-syntax-pattern` and construct a kv-pair
   where the key is the include-tag that was matched
   and the value is the render-tag it should be replaced with."
  [[match match-attr]]
  (let [[template-name & attrs] (str/split match-attr #"\s+")
        attr-kvs (->> attrs
                      (map #(->> (str/split % #"=") (str/join ": ")))
                      (cons (-> template-name (str/replace "\"" "") (wrap-with-str "\"")))
                      (str/join ", "))]
    [match (str "{% render " attr-kvs " %}")]))

(defn replace-deprecated-parameters
  "The render tag doesn't assume parameters to be contained in an `include` object."
  [include-content]
  (-> include-content
      (str/replace #"\{[^}]*?\binclude\.\b[^}]*?\}"
                   #(str/replace % #"\binclude\.\b" ""))))

(defn replace-deprecated-tag
  "The Liquid include syntax is deprecated and therefor being replaced by `render` syntax."
  [template]
  (->> template
       (re-seq include-syntax-pattern)
       (map include->render)
       (reduce (fn [carry [match replacement]]
                 (str/replace carry match replacement)) template)))
