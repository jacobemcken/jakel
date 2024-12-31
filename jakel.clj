#!/usr/bin/env bb
(ns jakel
  (:require [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [frontmatter]
            [wet.core :as wet])
  (:import (java.io File)))

(defn ensure-trailing-slash
  [path]
  (cond-> path
    (not (str/ends-with? path "/"))
    (str "/")))

(def cli-options
  [["-s" "--source DIR" "Directory where files are read from." :default "./" :parse-fn ensure-trailing-slash]
   ["-d" "--destination DIR" "Directory where site files are written." :default "./_site/" :parse-fn ensure-trailing-slash]
   ["-c" "--config FILE" "Alternative config file." :default "_config.yml"]
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
  "Takes a page which is a map with a body and optional frontmatter,
   a Liquid context which must match wet.core/render options.
   The `liquid-context` contains templates required by `render` tags (Wet specific).
   Layouts also contain similar templates but they are only used in a Jakel context (frontmatter)."
  [page liquid-context layouts]
  (loop [layout-name (get-in page [:frontmatter :layout])
         content (get page :body)]
    (if-not layout-name
      content
      (let [{:keys [frontmatter body]} (get layouts layout-name)]
        (recur (:layout frontmatter)
               (wet/render body (update liquid-context :params
                                        assoc :content content :layout frontmatter)))))))

(defmethod parse :html
  [^File file ctx]
  (println "- HTML template")
  (let [{:keys [body frontmatter]} (frontmatter/parse (slurp file))
        liquid-context (update (:liquid ctx) :params merge frontmatter)
        content (-> body
                    (wet/parse)
                    (wet/render liquid-context))]
    (if-let [layout (:layout frontmatter)]
      (wet/render (get-in ctx [:layouts layout :body])
                  (assoc-in liquid-context [:params :content] content))
      content)))

(defmethod parse :md
  [^File file ctx]
  (println "- Markdown template")
  (let [page (frontmatter/parse (slurp file))]
    (apply-layouts page
                   (assoc-in (:liquid ctx) [:params :page] (:frontmatter page))  ;; TODO add date
                   (:layouts ctx))))

(defmethod parse :default
  [^File file _ctx]
  (println "- No preprocessing")
  (when (.isFile file)
    (slurp file)))

(defn main
  [& args]
  (let [{:keys [options arguments _summary]} (cli/parse-opts args cli-options)
        [command] arguments]
    (when (not= "build" command)
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
                                              [(.getName file)
                                               (parse file {:layouts layouts
                                                            :liquid {:params {:site config}
                                                                     :templates includes}})])})
          _ (println "Read posts\n" (keys posts))]

      (println "\nbuilding!")
      (let [files (process-files (:source options)
                                 {:filter-fn filter-fn
                                  :process-fn (fn [relative-path file]
                                                [(.toString relative-path)
                                                 (parse file {:layouts layouts
                                                              :liquid {:params {:site config
                                                                                :paginator {:posts [{:url "http://something/" :title "My post"}
                                                                                                    {:url "http://somethingelse/" :title "Another post"}]}}
                                                                       :templates includes}})])})]
        (println (get files "index.html"))
        (println (keys files))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply main *command-line-args*))
