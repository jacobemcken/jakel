(ns jakel.utils
    (:require [clojure.java.io :as io]
              [wet.core :as wet]))

(defn str->input-stream
  [^String s]
  (io/input-stream (.getBytes s "UTF-8")))

(defn apply-layouts
  "Takes a template which is a map with a body and optional frontmatter,
   a Liquid context which must match wet.core/render options.
   The template data structure looks as the following:

       {:frontmatter {:layout \"default\" :title \"Some title\"}
        :body \"some template\"}

   The `liquid-context` contains templates required by `render` tags (Wet specific).
   Layouts also contain similar templates but they are only used in a Jakel context (frontmatter)."
  [initial-template liquid-context layouts]
  (loop [layout-name (get-in initial-template [:frontmatter :layout])
         template initial-template]
    (if-not layout-name
      (update template :body str->input-stream)
      (let [{:keys [frontmatter body]} (get layouts layout-name)]
        (recur (:layout frontmatter)
               (->> (wet/render body (update liquid-context :params
                                             assoc :content (:body template) :layout frontmatter))
                    (assoc template :body)))))))
