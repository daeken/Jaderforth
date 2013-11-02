(ns jaderforth.core
  (:require [clojure.string :as string])
  (:use [clojure.core.match :only (match)])
  (:gen-class))

(defn parse [code]
  (string/split code #"\s+"))

(defn append-token [words word token]
  (assoc words word (conj (get words word []) token)))

(defn sub [words word-stack comment-depth code]
  (match [comment-depth (first code)]
         [0 ":"]   (recur (append-token words (second code) :word) (cons (second code) word-stack) 0 (drop 2 code))
         [0 ":m"]  (recur (append-token words (second code) :macro) (cons (second code) word-stack) 0 (drop 2 code))
         [0 ";"]   (recur words (rest word-stack) 0 (rest code))
         [_ "("]   (recur (append-token words (first word-stack) "(") word-stack (inc comment-depth) (rest code))
         [_ ")"]   (recur (append-token words (first word-stack) ")") word-stack (dec comment-depth) (rest code))
         [_ nil]   words
         :else (recur (append-token words (first word-stack) (first code)) word-stack 0 (rest code))))

(def wordify
  (partial sub {} '("main") 0))

(def process
  (comp wordify parse))

(def default-input
  ": someword ( foo bar ) baz :m test ( a b ) a b + ; hax ;\n5 6 test")

;;******************************************************************************
;;  Main entry point
;;******************************************************************************

(defn -main [& args]
  (process (or (first args) default-input)))

;;******************************************************************************
;;  Dev helpers
;;******************************************************************************

(def default-output
  {"main" ["5" "6" "test"], "test" [:macro "(" "a" "b" ")" "a" "b" "+"], "someword" [:word "(" "foo" "bar" ")" "baz" "hax"]})

(defn still-works? []
  (assert (= (process default-input) default-output)))

(comment
  (use '[clojure.tools.trace])
  (trace-ns 'jaderforth.core)
  (untrace-ns 'jaderforth.core))
