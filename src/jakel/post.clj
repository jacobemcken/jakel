(ns jakel.post
  (:require [clojure.string :as str]
            [jakel.utils :as utils]
            [markdown.core :as md]))

(defn enrich
  "Takes a post and the source file, and enrich the post with `:url`, `:date` & `:out-file`."
  [post file-name]
  (let [placeholders (utils/get-placeholders (get-in post [:params :page]) file-name)
        placeholders-str-keys (update-keys placeholders str)
        url (str/replace "/:categories/:year/:month/:day/:title/" utils/placeholders-re placeholders-str-keys)]
    (-> post
        (update-in [:params :page] #(assoc % :date (:date placeholders) :url url))
        ; Unsure if placeholders are needed elsewhere and if keys should be strings or keywords
        (assoc :placeholders placeholders-str-keys)
        (assoc :out-file (str (subs url 1) "index.html")))))

(defn extract-markdown-links
  [text]
  (->> text
       str/split-lines
       (filter #(re-matches #"^\[[^\]]+\]:.*" %))))

(defn add-excerpt
  "The excerpt according to Jekyll:
  > By default this is the first paragraph of content in the post...

  Source: https://jekyllrb.com/docs/posts/"
  [post]
  (let [split-pattern (or (some-> (get-in post [:params :page :excerpt_separator])
                                  (re-pattern))
                          #"\n\n")
        [excerpt remaining] (str/split (:body post) split-pattern 2)]
    (update-in post [:params :page] assoc
               :excerpt (->> (extract-markdown-links (or remaining ""))
                             (cons excerpt)
                             (str/join "\n"))
               :content (:body post))))

(defn finalize
  "Takes a raw (Markdown) Page or Post and adds excerpt and converts Markdown to HTML."
  [page-or-post]
  (-> page-or-post
      (add-excerpt)
      (update :body md/md-to-html-string :reference-links? true)
      (update-in [:params :page :excerpt] md/md-to-html-string :reference-links? true)))