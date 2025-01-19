#!/usr/bin/env bb
(ns jakel
  (:require [babashka.http-server :as http-server]
            [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [frontmatter]
            [jakel.utils :as utils]
            [markdown.core :as md]
            [pagination]
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

(defn convert->liquid-structure
  [page at-path]
  (-> page
      (dissoc :frontmatter)
      (assoc :layout (get-in page [:frontmatter :layout]))
      (assoc-in (concat [:params] at-path) (:frontmatter page))))

(defn prepare
  "Takes a Liquid file and returns a map with params and a wet template:

       {:layout \"some_layout\"
        :params {:at-path-key {:title \"Some title\" ...}}
        :body \"Wet template (Liquid)\"}
   
   Depending on the context the frontmatter params should be placed in different keys."
  ;; E.g. post & pages expect `:page`, layouts expect `:layout` & paginator plugin expects `:paginate` among others
  ;; templates doesn't set any params (hence the possibility for nil)
  [^File file & at-path]
  (-> file
      slurp
      frontmatter/parse
      (convert->liquid-structure at-path)
      (update :body wet/parse)))

(defmulti parse
  (fn [^File file _ctx]
    (get-ext-key (.getName file))))

(defmethod parse :html
  [^File file ctx]
  (println "- HTML template")
  (let [page (prepare file)
        liquid-context (update (:liquid ctx) :params merge (:params page))]
    (-> (update page :body wet/render liquid-context)
        (utils/apply-layouts liquid-context (:layouts ctx)))))

(defn string-to-instant
  "Jekyll defaults to 12 midday."
  [date-str]
  (java.time.Instant/parse (str date-str "T12:00:00Z")))

(defn enrich-post
  "Takes a post and the source file, and enrich the post with `:url`, `:date` & `:out-file`."
  [post file-name]
  (let [path-without-ext (str/replace file-name #"\.(markdown|md)$" "")]
    (update-in post [:params :page]
               ;; Fallback values if not in frontmatter
               #(merge {:date (when-let [date-str (re-find #"^\d{4}-\d{2}-\d{2}" file-name)]
                                (string-to-instant date-str))
                        :out-file (str path-without-ext "/index.html")
                        :url (str path-without-ext "/")} %))))

(defn add-excerpt
  "The excerpt according to Jekyll:
  > By default this is the first paragraph of content in the post...

  Source: https://jekyllrb.com/docs/posts/
  
  Excerpt is identifed after HTML covertion, to avoid
  parsing Markdown twice and having to find link references
  \"behind\" excerpt separator."
  [post]
  (let [split-pattern (or (some-> (get-in post [:params :page :excerpt_separator])
                                  (re-pattern))
                          #"(?<=</p[^>]*>)")] ; use fancy "lookbehind" to keep the closing tag
    (update-in post [:params :page] assoc
               :excerpt (-> (:body post)
                            (str/split split-pattern 2)
                            first)
               :content (:body post))))

(defmethod parse :md
  [^File file ctx]
  (println "- Markdown template")
  (let [page (-> (slurp file)
                 (frontmatter/parse)
                 (convert->liquid-structure [:page])
                 (enrich-post (.getName file))
                 (update :body md/md-to-html-string :reference-links? true)
                 (add-excerpt))
        liquid-context (update (:liquid ctx) :params merge (:params page))]
    (-> page
        (utils/apply-layouts liquid-context (:layouts ctx)))))

(defmethod parse :default
  [^File file _ctx]
  (println "- No preprocessing")
  (when (.isFile file)
    {:body (io/input-stream file)}))

(defn write-content
  [content-input-stream file-name]
  (let [out-file (io/file file-name)]
    (io/make-parents out-file)
    (with-open [in content-input-stream
                out (io/output-stream out-file)]
      (io/copy in out))))

(def date-formatter
  (.withZone (DateTimeFormatter/ofPattern "dd LLL uuuu")
             (ZoneId/systemDefault)))

(defn date-to-string
  [instant & _args]
  (.format date-formatter instant))

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
                                                [(strip-extension (.getName file)) (prepare file [:layout])])})
          _ (println "Read layouts\n" (keys layouts))
          includes (process-files (str (:source options) "_includes")
                                  {:process-fn (fn [_relative-path file]
                                                 [(.getName file) (prepare file)])})
          liquid-context {:params {:site config}
                          :templates includes
                          :filters jekyll-filters}

          _ (println "Read includes\n" (keys includes))
          posts (process-files (str (:source options) "_posts")
                               {:process-fn (fn [_relative-path file]
                                              [(str/replace (.getName file) #"\.(markdown|md)$" "/index.html")
                                               (parse file {:layouts layouts
                                                            :liquid liquid-context})])})
          _ (println "Read posts\n" (keys posts))]

      (println "\nbuilding!")
      (let [paginated-pages (pagination/paginator (vals posts) {:per_page (:paginate config)})
            index-pages (pagination/generator (rest paginated-pages)
                             (prepare (io/file (str (:source options) "index.html")) [:page])
                             liquid-context
                             layouts)
            files (process-files (:source options)
                                 {:filter-fn filter-fn
                                  :process-fn (fn [relative-path file]
                                                [(.toString relative-path)
                                                 (parse file {:layouts layouts
                                                              :liquid (-> liquid-context
                                                                          (update :params assoc :paginator (first paginated-pages)))})])})]
        (doseq [[file-name content] (concat files posts index-pages)]
          (write-content (:body content) (str (:destination options) file-name)))

        (when (= "serve" command)
          (println "Serving assets from the directory" (:destination options))
          (http-server/exec {:port (:port options) :dir (:destination options)}))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply main *command-line-args*))
