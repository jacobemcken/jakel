(ns jakel.plugins.relative-links
  "https://github.com/benbalter/jekyll-relative-links"
  (:refer-clojure :exclude [replace])
  (:require [clojure.string :as str]))

(defn replace
  "The render tag doesn't assume parameters to be contained in an `include` object."
  [body relative-links]
  (-> body
      (str/replace #"\[.*?\]\(([^\)]+)\)|\[.*?\]:\s*([^\s]+)"
                   (fn [[match url1 url2]]
                     (let [url (or url1 url2)]
                       (println (str "'" match "' and URL: " url))
                       (str/replace match (re-pattern url) (get relative-links url url)))))))
