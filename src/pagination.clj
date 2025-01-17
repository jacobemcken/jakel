(ns pagination
  "Jekyll docs: https://jekyllrb.com/docs/pagination/
   
   GitHub: https://github.com/sverrirs/jekyll-paginate-v2"
  (:require [clojure.math :as math]
            [clojure.string :as str]
            [jakel.utils :as utils]
            [wet.core :as wet]))

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
            :previous_page previous-page)
     remaining-posts]))

(defn page-path
  [permalink num]
  (when num
    (str/replace permalink #":num" (str num))))

(defn add-paths
  [{:keys [next_page previous_page] :as page} permalink]
  (assoc page
         :next_page_path (page-path permalink next_page)
         :previous_page_path (page-path permalink previous_page)))

(defn paginator
  "Default configuration: https://github.com/sverrirs/jekyll-paginate-v2/blob/master/lib/jekyll-paginate-v2/generator/defaults.rb

   Available values: https://github.com/sverrirs/jekyll-paginate-v2/blob/master/README-GENERATOR.md"
  [all-posts {:keys [limit per_page permalink offset sort_field sort_reverse] :as _conf
              :or {limit 0
                   per_page 10
                   permalink "/page:num/"
                   offset 0
                   sort_field "date"
                   sort_reverse false}}]
  (let [total-posts (count all-posts)
        sort-by-k (keyword sort_field)
        compare-fn (if sort_reverse compare #(compare %2 %1))]
    (loop [posts (->> all-posts
                      (map :frontmatter)
                      (sort-by sort-by-k compare-fn)
                      (drop offset)
                      (take (if (pos? limit) limit total-posts)))
           attr {:page 0
                 :per_page per_page
                 :total_posts total-posts
                 :total_pages (int (math/ceil (/ total-posts per_page)))}
           pages []]
      (if-not (seq posts)
        pages
        (let [[page remaining-posts] (pagination-page posts attr)]
          (recur remaining-posts
                 (add-paths page permalink)
                 (conj pages page)))))))

(defn generator
  [paginated-pages template liquid-context layouts]
  (->> paginated-pages
       (map (fn [page]
              [(str "page" (:page page) "/")
               (-> template
                   (update :body wet/render (-> liquid-context
                                                (update :params assoc :paginator page)))
                   (utils/apply-layouts liquid-context layouts))]))
       (into {})))
