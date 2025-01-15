#!/usr/bin/env bb
(ns jakel
  (:require [babashka.http-server :as http-server]
            [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [frontmatter]
            [markdown.core :as md]
            [wet.core :as wet])
  (:import (java.io File)
           (java.time ZoneId)
           (java.time.format DateTimeFormatter)))

(defn ensure-trailing-slash
  [path]
  (cond-> path
    (not (str/ends-with? path "/"))
    (str "/")))

(def cli-options
  [["-s" "--source DIR" "Directory where files are read from." :default "./" :parse-fn ensure-trailing-slash]
   ["-d" "--destination DIR" "Directory where site files are written." :default "./_site/" :parse-fn ensure-trailing-slash]
   ["-c" "--config FILE" "Alternative config file." :default "_config.yml"]
   ["-P" "--port PORT" " Local server port. Listen on the given port. The default is `4000`." :default 4000 :parse-fn #(Integer/parseInt %)]
   ["-h" "--help"]])

(defn usage
  []
  (str "Usage: jakel build -d <dir>"))

(defn read-config
  [file]
  (-> file
      slurp
      yaml/parse-string))

; Article worth reading: https://mincong.io/2019/04/16/glob-expression-understanding/
(def glob-patterns
  ["[_.]**"
   "**/[_.]**"])

(defn match-globs-fn
  "Takes a list of GLOB patterns and
   return a functions that match agains all the patterns."
  [glob-patterns]
  (let [matcher-fns
        (->> glob-patterns
             (map #(let [matcher (.getPathMatcher
                                  (java.nio.file.FileSystems/getDefault)
                                  (str "glob:" %))]
                     (fn [path]
                       #_(println "Matching" path " -> " %)
                       (.matches matcher path)))))]
    (fn [relative-path]
      (some #(% relative-path) matcher-fns))))

(defn process-files
  "Takes string with a path to the dir that needs processing."
  [^String dir-path-str {:keys [filter-fn process-fn]
                         :or {filter-fn (constantly true)}}]
  {:pre  [(fn? process-fn)]}
  (let [dir (io/file dir-path-str)
        base-path (.toPath dir)]
    (->> dir
         file-seq
         rest
         (keep (fn [file]
                 (let [relative-path (.relativize base-path (.toPath file))]
                   (when (and (.isFile file)
                           (filter-fn relative-path))
                  (process-fn relative-path file)))))
         (into {}))))

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
  (let [posts-frontmatter-only (map (comp :frontmatter val) posts)
        [paginator _] (paginate 5 posts-frontmatter-only 1)]
    paginator))

(defn strip-extension
  [file-name]
  (str/replace file-name #"\.([^.]+)$" ""))

(defn get-ext-key
  [file-name]
  (some->> file-name
           (re-find #"^(.+)(\.([^.]+))$")
           last
           str/lower-case
           keyword))

(defn str->input-stream
  [^String s]
  (io/input-stream (.getBytes s "UTF-8")))

(defn prepare
  "Takes a file and returns a map with frontmantter and a wet template in body:
   {:frontmatter {:title \"Some title\" ...} :body Wet template (Liquid)}"
  [^File file]
  (-> file
      slurp
      frontmatter/parse
      (update :body wet/parse)))

(defmulti parse
  (fn [^File file _ctx]
    (get-ext-key (.getName file))))

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
      template
      (let [{:keys [frontmatter body]} (get layouts layout-name)]
        (recur (:layout frontmatter)
               (->> (wet/render body (update liquid-context :params
                                             assoc :content (:body template) :layout frontmatter))
                    (assoc template :body)))))))

(defmethod parse :html
  [^File file ctx]
  (println "- HTML template")
  (let [page (prepare file)
        liquid-context (assoc-in (:liquid ctx) [:params :page] (:frontmatter page))]
    (-> (update page :body wet/render liquid-context)
        (apply-layouts liquid-context (:layouts ctx))
        (update :body str->input-stream))))

(defn add-excerpt
  "The excerpt according to Jekyll:
  > By default this is the first paragraph of content in the post...

  Source: https://jekyllrb.com/docs/posts/
  
  Excerpt is identifed after HTML covertion, to avoid
  parsing Markdown twice and having to find link references
  \"behind\" excerpt separator."
  [post]
  (let [split-pattern (or (some-> (get-in post [:frontmatter :excerpt_separator])
                                  (re-pattern))
                          #"(?<=</p[^>]*>)")] ; use fancy "lookbehind" to keep the closing tag
    (assoc-in post [:frontmatter :excerpt]
              (-> (:body post)
                  (str/split split-pattern 2)
                  first))))

(defmethod parse :md
  [^File file {:keys [enrich] :as ctx :or {enrich identity}}]
  (println "- Markdown template")
  (let [page (-> (slurp file)
                 (frontmatter/parse)
                 enrich ; TODO frontmatter should have priority
                 (update :body md/md-to-html-string :reference-links? true)
                 (add-excerpt))
        liquid-context (assoc-in (:liquid ctx) [:params :page] (:frontmatter page))]
    (-> page
        (apply-layouts liquid-context (:layouts ctx))
        (update :body str->input-stream))))

(defmethod parse :default
  [^File file _ctx]
  (println "- No preprocessing")
  (when (.isFile file)
    {:body (io/input-stream file)}))

(defn write-content
  [file-name content-input-stream]
  (let [out-file (io/file file-name)]
    (io/make-parents out-file)
    (with-open [in content-input-stream
                out (io/output-stream out-file)]
      (io/copy in out))))

(defn string-to-instant
  "Jekyll defaults to 12 midday."
  [date-str]
  (java.time.Instant/parse (str date-str "T12:00:00Z")))

(def date-formatter
  (.withZone (DateTimeFormatter/ofPattern "dd LLL uuuu")
             (ZoneId/systemDefault)))

(defn date-to-string
  [instant & _args]
  (.format date-formatter instant))

(defn enrich-post
  "Takes a post and the source file, and enrich the post with `:url`, `:date` & `:out-file`."
  [post file-name]
  (let [date-str (re-find #"^\d{4}-\d{2}-\d{2}" file-name)
        path-without-ext (str/replace file-name #"\.(markdown|md)$" "")]
    (update post :frontmatter
            assoc
            :date (string-to-instant date-str)
            :out-file (str path-without-ext "/index.html")
            :url (str path-without-ext "/"))))

(def jekyll-filters
  "Returns a map of Jekyll specific filters (not part of Liquid)"
  {:date_to_string date-to-string})

(defn main
  [& args]
  (let [{:keys [options arguments _summary]} (cli/parse-opts args cli-options)
        [command] arguments]
    (when-not (contains? #{"build" "serve"} command)
      (println (usage))
      (System/exit 2))

    (let [filter-fn (complement (match-globs-fn glob-patterns))
          config (read-config (str (:source options) (:config options)))
          layouts (process-files (str (:source options) "_layouts")
                                 {:process-fn (fn [_relative-path file]
                                                [(strip-extension (.getName file)) (prepare file)])})
          _ (println "Read layouts\n" (keys layouts))
          includes (process-files (str (:source options) "_includes")
                                  {:process-fn (fn [_relative-path file]
                                                 [(.getName file) (prepare file)])})
          _ (println "Read includes\n" (keys includes))
          posts (process-files (str (:source options) "_posts")
                               {:process-fn (fn [_relative-path file]
                                              [(str/replace (.getName file) #"\.(markdown|md)$" "/index.html")
                                               (parse file {:layouts layouts
                                                            :enrich #(enrich-post % (.getName file))
                                                            :liquid {:params {:site config}
                                                                     :templates includes
                                                                     :filters jekyll-filters}})])})
          _ (println "Read posts\n" (keys posts))]

      (println "\nbuilding!")
      (let [files (process-files (:source options)
                                 {:filter-fn filter-fn
                                  :process-fn (fn [relative-path file]
                                                [(.toString relative-path)
                                                 (parse file {:layouts layouts
                                                              :liquid {:params {:site config
                                                                                :paginator (get-paginator posts)}
                                                                       :templates includes
                                                                       :filters jekyll-filters}})])})]
        (doseq [[file-name content] (concat files posts)]
          (write-content (str (:destination options) file-name) (:body content)))

        (when (= "serve" command)
          (println "Serving assets from the directory" (:destination options))
          (http-server/exec {:port (:port options) :dir (:destination options)}))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply main *command-line-args*))
