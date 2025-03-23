(ns jakel.jekyll-filters
  "Jekyll adds filters to the built-in from Liquid:

   https://jekyllrb.com/docs/liquid/filters/"
  (:require [clojure.string :as str])
  (:import (java.time ZoneId)
           (java.time.format DateTimeFormatter)))

(def date-formatter
  (.withZone (DateTimeFormatter/ofPattern "dd LLL uuuu")
             (ZoneId/systemDefault)))

(def date-formatter-ordinal
  (.withZone (DateTimeFormatter/ofPattern "d,LLL,uuuu")
             (ZoneId/systemDefault)))

(defn get-ordinal-suffix
  [day-of-month]
  (if (<= 11 day-of-month 13)
    "th"
    (case (mod day-of-month 10)
      1 "st"
      2 "nd"
      3 "rd"
      "th")))

(defn date-to-string
  "https://github.com/jekyll/jekyll/blob/1f319fb273b6cdf876bc6edd38d7477935cdda8c/lib/jekyll/filters/date_filters.rb#L75"
  [instant & [type style]]
  (if-not (= "ordinal" type)
    (.format date-formatter instant)
    (let [[day month-type year]
          (-> (.format date-formatter-ordinal instant)
              (str/split #","))
          ordinal-day (str day (get-ordinal-suffix (Integer. day)))]
      (if (= "US" style)
        (str month-type " " ordinal-day ", " year)
        (str ordinal-day " " month-type " " year)))))

(def jekyll-filters
  "Returns a map of Jekyll specific filters (not part of Liquid)"
  {:date_to_string date-to-string})