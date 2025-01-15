(ns pagination
  "Jekyll docs: https://jekyllrb.com/docs/pagination/
   
   GitHub: https://github.com/sverrirs/jekyll-paginate-v2")

(defn paginate
  [pagination all-posts page-no]
  (let [posts (take pagination all-posts)
        remaining-posts (seq (drop pagination all-posts))]
    [{:page page-no
      :posts posts
      :next-page (when remaining-posts (+ 1 page-no))
      :previous-page (when (> page-no 1) (dec page-no))}
     remaining-posts]))

(defn get-paginator
  [posts]
  (let [posts-frontmatter-only (->> posts
                                    (map :frontmatter)
                                    (sort-by :date #(compare %2 %1)))
        [paginator _] (paginate 5 posts-frontmatter-only 1)]
    paginator))