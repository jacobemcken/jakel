#!/usr/bin/env bb
(ns jakel
  (:require [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]))

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

(defn main
  [& args]
  (let [{:keys [options arguments _summary]} (cli/parse-opts args cli-options)
        [command] arguments]
    (when (not= "build" command)
      (println (usage))
      (System/exit 2))

    (println "building!")
    (println (read-config (str (:source options) (:config options))))))

(apply main *command-line-args*)
