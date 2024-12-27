#!/usr/bin/env bb
(ns jakel
  (:require [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [frontmatter])
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

(defn get-ext-key
  [file-name]
  (some->> file-name
           (re-find #"^(.+)(\.([^.]+))$")
           last
           str/lower-case
           keyword))

(defmulti parse
  (fn [^File file]
    (get-ext-key (.getName file))))

(defmethod parse :html
  [^File file]
  (println "- HTML template")
  (frontmatter/parse (slurp file)))

(defmethod parse :default
  [^File file]
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
    
    (let [filter-fn (complement (match-globs-fn glob-patterns))]
      (println "\nbuilding!")
      (let [files (process-files (:source options) {:filter-fn filter-fn
                                                    :process-fn (fn [relative-path file]
                                                                  [(.toString relative-path) (parse file)])})]
        (println (keys files))))

    (println (read-config (str (:source options) (:config options))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply main *command-line-args*))
