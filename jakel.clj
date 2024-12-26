#!/usr/bin/env bb
(ns jakel
  (:require [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [frontmatter]))

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

(defn find-eligible-files
  [^String dir-path-str]
  (let [dir (io/file dir-path-str)
        base-path (.toPath dir)
        match-fn (match-globs-fn glob-patterns)]
    (->> dir
         file-seq
         rest
         (remove #(match-fn (.relativize base-path (.toPath %)))))))

(defn parse-html
  [file]
  (println "parse html")
  (let [{:keys [frontmatter body]} (frontmatter/parse (slurp file))]
    (println "frontmatter" frontmatter)
    ))

(def special-parsing
  {:html parse-html})

(defn main
  [& args]
  (let [{:keys [options arguments _summary]} (cli/parse-opts args cli-options)
        [command] arguments]
    (when (not= "build" command)
      (println (usage))
      (System/exit 2))

    (println "building!")
    (let [files (find-eligible-files (:source options))]
      (doseq [file files]
        (println "Parsing:" file)
        (let [ext (some->> (.getName file)
                           (re-find #"^(.+)(\.([^.]+))$")
                           last
                           keyword)]
          (if (contains? #{:html} ext)
            (do (println "- Special parse")
                ((special-parsing ext) file))
            (println "- Just copy")))))

    (println (read-config (str (:source options) (:config options))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply main *command-line-args*))
