(ns crazyfuck.core
  (:require [clojure.java.io :as io])
  (:gen-class
    :main true))

(def output (atom ""))

(defn move-point [ope spec spec-point memory memory-point]
  (let [n (ope memory-point)
        point (nth memory n nil)
        new-memory (if (nil? point) (concat memory (list 0)) memory )]
    (list spec (inc spec-point) new-memory n )))

(defn num-ope [ope spec spec-point memory memory-point]
  (let [new-memory (flatten [(take memory-point memory) (ope (nth memory (int memory-point))) (nthrest memory (+ 1 memory-point))])]
    (list spec (inc spec-point) new-memory memory-point)))

(defn input-point [_ spec spec-point memory memory-point]
  (let [new-memory (flatten [(take memory-point memory) (int (.charAt (read-line) 0)) (nthrest memory  (+ 1 memory-point))])]
    (list spec (inc spec-point) new-memory memory-point)))

(defn output-point [_ spec spec-point memory memory-point]
  (do (print (char (nth memory memory-point)))
      (swap! output #(str % %2) (char (nth memory memory-point)))
  (list spec (inc spec-point) memory memory-point)))

(defn jump-point [c spec spec-point memory memory-point]
  (let [com {\[ 1 , \] -1 }
        ope (if (> c 0) inc dec)]
    (letfn [(f [point n]
              (if (zero? n) point (recur (ope point) (+ n (get com (get spec point) 0)))))]
      (list spec (inc (if (#(if (> 0 c) (zero? %) (not= 0 %)) (nth memory memory-point)) spec-point (f (ope spec-point) c))) memory memory-point))))

(def commands {\> (list move-point inc)
               \< (list move-point dec)
               \+ (list num-ope inc)
               \- (list num-ope dec)
               \, (list input-point)
               \. (list output-point)
               \[ (list jump-point 1)
               \] (list jump-point -1)
               nil (list nil)})

(defn operation [[spec spec-point memory memory-point]]
  (let [command (get spec spec-point)
        ope (get commands command (list :none))
        [fun arg] (list (first ope) (second ope))]
    (println " NAIYOU " command spec-point memory memory-point)
    (case fun
      nil (println "output\n" @output "\nfinish")
      :none (recur (list spec (inc spec-point) memory memory-point))
      (recur (fun arg spec spec-point memory memory-point)))))

(defn check-spec? [spec]
  (let [com {\[ 1 , \] -1 }]
    ((fn [spec c]
     (let [start (first spec)
           n (+ c (get com start 0 ))]
       (if (nil? start)
         (if (= 0 n) true false)
         (if (> 0 n) false (recur (rest spec) n))))) spec 0 )))

(defn start [spec]
  (if (check-spec? spec) (operation (list spec 0 (list 0) 0))
    (println "---------- KAKKO AWASERO ----------")))

(defn -main [& args]
  (let [target (first args)]
    (if (nil? target)
      (do (println "Please Input") (start (read-line)))
      (start ()))))
