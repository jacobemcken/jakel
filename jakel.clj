#!/usr/bin/env bb
(ns jakel
  (:require [clojure.tools.cli :as cli]))

(def cli-options
  [["-s" "--source DIR" "Directory where files are read from." :default "./"]
   ["-d" "--destination DIR" "Directory where site files are written." :default "./_site/"]
   ["-c" "--config FILE" "Alternative config file." :default "_config.yml"]
   ["-h" "--help"]])

(defn usage
  []
  (str "Usage: jakel build -d <dir>"))

(defn main
  [& args]
  (let [{:keys [_options arguments _summary]} (cli/parse-opts args cli-options)
        [command] arguments]
    (when (not= "build" command)
      (println (usage))
      (System/exit 2))
    (println "building!")))

(apply main *command-line-args*)
