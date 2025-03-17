(ns frontmatter
  "Copy-pasted from: https://github.com/liquidz/frontmatter under EPL"
  (:require
   [clojure.string :as str]
   [clj-yaml.core :as yaml]))

(defn- split-lines
  [lines delim]
  (let [x (take-while #(not= delim %) lines)]
    (list x (drop (+ 1 (count x)) lines))))

(defn- parse-yaml
  [s]
  (yaml/parse-string s))

(def select-parse-fn
  {"---" parse-yaml})

(defn parse
  [original-body]
  (let [[first-line & rest-lines] (str/split-lines original-body)
        [frontmatter body]        (split-lines rest-lines first-line)]
    (if-let [parser (select-parse-fn first-line)]
      {:body (str/join "\n" body)
       :frontmatter (parser (str/join "\n" frontmatter))}
      {:frontmatter {} :body original-body})))
