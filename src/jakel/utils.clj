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
  (loop [layout-name (:layout initial-template)
         template initial-template]
    (if-not layout-name
      template
      (let [{:keys [body params layout]} (get layouts layout-name)]
        (recur layout
               (->> (wet/render body (update liquid-context :params
                                             assoc :content (:body template) :layout params))
                    (assoc template :body)))))))
