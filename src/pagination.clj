(ns pagination
  "Jekyll docs: https://jekyllrb.com/docs/pagination/
   
   GitHub: https://github.com/sverrirs/jekyll-paginate-v2"
  (:require [clojure.math :as math]))

(defn pagination-page
  [posts attr]
  (let [page-posts (take (:per_page attr) posts)
        remaining-posts (seq (drop (:per_page attr) posts))
        page-no (inc (:page attr))
        next-page (when remaining-posts
                    (inc page-no))
        previous-page (when (> page-no 1)
                        (dec page-no))]
    [(assoc attr
            :page page-no
            :posts page-posts
            :next_page next-page
            :next_page_path (when next-page
                              (str "page" page-no "/"))
            :previous_page previous-page
            :next_page_path (when previous-page
                              (str "page" page-no "/")))
     remaining-posts]))

(defn paginator
  [all-posts conf]
  (let [size (or (:size conf) 12)
        total-posts (count all-posts)]
    (loop [posts (->> all-posts
                      (map :frontmatter)
                      (sort-by :date #(compare %2 %1)))
           attr {:page 0
                 :per_page size
                 :total_posts total-posts
                 :total_pages (int (math/ceil (/ total-posts size)))}
           pages []]
      (if-not (seq posts)
        pages
        (let [[page remaining-posts] (pagination-page posts attr)]
          (recur remaining-posts page (conj pages page)))))))
