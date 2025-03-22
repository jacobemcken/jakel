#!/usr/bin/env bb
(ns jakel
  (:require [babashka.http-server :as http-server]
            [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [frontmatter]
            [jakel.include-support :as include-support]
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
   "**/[_.]**"
   "vendor/**/*"
   "Gemfile*"])

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
      (update :body (comp wet/parse include-support/replace-deprecated-tag))))

(defn load-include
  [^File file]
  (-> file
      slurp
      include-support/replace-deprecated-tag
      include-support/replace-deprecated-parameters
      wet/parse))

(defmulti parse
  (fn [^File file]
    (get-ext-key (.getName file))))

(defmethod parse :html
  [^File file]
  (println "- HTML template")
  (prepare file))

(defn enrich-post
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
  [^File file]
  (println "- Markdown template")
  (-> (slurp file)
      (frontmatter/parse)
      (convert->liquid-structure [:page])
      (enrich-post (.getName file))
      (update :body md/md-to-html-string :reference-links? true)
      (add-excerpt)))

(defmethod parse :default
  [^File file]
  (println "- No preprocessing")
  (when (.isFile file)
    (io/input-stream file)))

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

(defmulti generate
  (fn [x _ctx]
    (cond
      (instance? java.io.InputStream x) :binary
      (map? x) :liquid ; will have :body, :layout & :params keys
      :else (throw (ex-info "Unknown source" {:x x})))))

(defmethod generate :binary
  [x _ctx]
  x)

(defmethod generate :liquid
  [liquid ctx]
  (let [liquid-context (update (:liquid ctx) :params merge (:params liquid))]
    (-> (update liquid :body wet/render liquid-context)
        (utils/apply-layouts liquid-context (:layouts ctx))
        :body
        utils/str->input-stream)))

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
                                                 [(.getName file) (load-include file)])})
          liquid-context {:params {:site config}
                          :templates includes
                          :filters jekyll-filters}

          _ (println "Read includes\n" (keys includes))
          posts (process-files (str (:source options) "_posts")
                               {:process-fn (fn [_relative-path file]
                                              (let [post (parse file)]
                                                [(:out-file post) post]))})
          _ (println "Read posts\n" (keys posts))]

      (println "\nbuilding!")
      (let [files (-> (:source options)
                      (process-files {:filter-fn filter-fn
                                      :process-fn (fn [relative-path file]
                                                    [(.toString relative-path)
                                                     (parse file)])})
                      (pagination/generator (vals posts) {:per_page (:paginate config)}))]
        (doseq [[file-name content] (concat files posts)]
          (-> (generate content {:layouts layouts :liquid liquid-context})
              (write-content (str (:destination options) file-name))))

        (when (= "serve" command)
          (println "Serving assets from the directory" (:destination options))
          (http-server/exec {:port (:port options) :dir (:destination options)}))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply main *command-line-args*))
