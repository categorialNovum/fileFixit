(ns filefixit.core
  (:gen-class))

(use 'clojure.zip)

;data format
;tree -> '(a (b (d) (e)) (c (f)))
;path -> '(a b e)

;numbers from file get read as chars, convert them to numbers
(defn to-numbers [line]
  (map read-string (re-seq #"[\d.]+" line)))

;problem record
(defrecord filesys [existing needed])

(def file (.split #"\n" (slurp "/Users/jbeaumon/code/clojure/filefixit/src/filefixit/fixit.in")))
(def numTests (Integer/parseInt (first file)))
(def tests (rest file))
(def a (take-last 34 tests))
(def h (to-numbers (first a)))
(def ex (first h))
(def nd (last h))
(def fs (filesys. (take ex (rest a)) (take nd (nthrest (rest a) ex))))
;;;;;;;;;;;;;;;
(def tree '(a (b (d) (e)) (c (f))))
(def path '(a b e))
(def p2 '(a b x))
(def leaves (rest tree))
(def r-path (rest path))

(defn split-path [path]
  (rest (.split #"\/" (str path))))

(defn next-jump [leaves next-dir]
  (if (or (empty? leaves) (nil? next-dir))
    nil
    (first (filter #(= (first %) next-dir) leaves))))

(defn contains-path? [tree path]
  (cond (empty? path)
          true
        (not= (first tree) (first path))
          false
;          ((println  tree) (println path))
        :else
          (recur (next-jump (rest tree) (second path)) (rest path))))

;(defn add-to-tree [tree path]

(defn build-tree [path-list tree]
  (if (or (empty? path-list) (contains-path? tree (first path-list)))
    tree
  (recur (rest path-list) (add-to-tree tree path))))

(defn print-fs [fs case-no]
  (println (str "Case #" case-no ": " ))
  (println "Existing:")
  (println (:existing fs))
  (println "Needed:") 
  (println (:needed fs))
  (println "-------------------------------"))

(defn process [work]
  (let [nm (.split #" " (first work))
       n (Integer/parseInt (first nm))
       m (Integer/parseInt (second nm)) 
       nLines (+ n m)
       paths (rest (take (inc nLines) work))
       exists (take n paths)
       needed (take-last m paths)] 
    (println  "--------------------------------------------------------------")
    (println  "Existing :")
    (println  exists)
    (println  "Needed :")
    (println  needed)
     needed))
    ;(recur (dec t) (nthrest work nLines)))))
    ;nLines)))
;
;itterate through tests
(defn run [data case-no]
  (if (empty? data)
    nil
    (let [header (to-numbers (first data))
          tests (rest data)
          n-exist (first header)
          n-needed (last header)
          fs (filesys. (take n-exist tests) (take n-needed (nthrest tests n-exist)))
          dummy (print-fs fs case-no)]
      (recur (nthrest tests (+ n-exist n-needed)) (inc case-no)))))


(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
;  (def p (split-path (first (process tests))))
;  (run tests 1))
  (contains-path? tree p2))
  
