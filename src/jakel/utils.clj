(ns jakel.utils
    (:require [clojure.java.io :as io]
              [clojure.string :as str]
              [wet.core :as wet])
  (:import (java.time Instant OffsetDateTime ZoneId)
           (java.time.format DateTimeFormatter)))

(def date-placeholder-formatter
  (.withZone (DateTimeFormatter/ofPattern "uuuu uu MM M MMM dd d DDD HH mm ss")
             (ZoneId/systemDefault)))

(def placeholders-re
  (->> '(:year :short_year :month :i_month :short_month :day :i_day :y_day :hour :minute :second :title :slug :categories)
       (str/join #"|")
       (re-pattern)))

(defn add-date-placeholders
  "Enriches with date placeholders:
   https://jekyllrb.com/docs/permalinks/#placeholders
   
   > Note that pages and collections (excluding posts and drafts) don’t have time ..."
  [page]
  (let [[year short_year month i_month short_month day i_day y_day hour minute second]
        (when (instance? Instant (:date page))
          (-> (.format date-placeholder-formatter (:date page))
              (str/split #" ")))]
    (-> {:year year
         :short_year short_year
         :month month
         :i_month i_month
         :short_month short_month
         :day day
         :i_day i_day
         :y_day y_day
         :hour hour
         :minute minute
         :second second}
        (merge page))))

(defn string-to-instant
  "Jekyll defaults to 12 midday."
  [date-str]
  (java.time.Instant/parse (str date-str "T12:00:00Z")))

(def date-frontmatter-formatter
  (.withZone (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss X")
             (ZoneId/systemDefault)))

(defn get-placeholders
  "The docs is a bit vauge about how `:categories`, `:title` and `:slug`
   should behave, and if pages and posts behave the same:
   https://jekyllrb.com/docs/permalinks/

   Implementation is done by observing a running system - which is imprecise."
  [page file-name]
  ; Post follow the format: YEAR-MONTH-DAY-title.MARKUP - https://jekyllrb.com/docs/posts/
  (let [[_ date-str title _markup] (re-find #"^(\d{4}-\d{2}-\d{2})-(.*)\.([^.]*)$" file-name)
        slugified-title (str/replace title #"\s+" "-")]
    (-> page
        (select-keys [:categories :date :title :slug])
        ; From docs: Also Jekyll automatically parses out double slashes in the URLs, so if no categories are present, it will ignore this.
        (update :categories #(->> % (filter not-empty) (map str/lower-case) (str/join "/")))
        (update :date #(if %
                         (-> %
                             (OffsetDateTime/parse date-frontmatter-formatter)
                             (.toInstant))
                         (string-to-instant date-str)))
        (assoc :title (or (:slug page) slugified-title))
        (update :slug #(or % slugified-title)) ; (str/replace  #"[^a-zA-Z0-9\-]" "")
        (add-date-placeholders)
        (update-vals #(or % ""))))) ; Render template cause NullPointerException for nil values

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
