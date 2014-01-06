(ns org-criterium.core
  (:require [criterium.core :refer :all :exclude [bench]]))

(defn scale-for
  "Scales time for multiple numbers based on the average."
  [nums]
  (println :nums nums)
  (let [avg (/ (apply + nums) (count nums))]
    (scale-time avg)))

(def line        "| %16s | %14s | %14s | %14s | %14s |")
(def result-line "| %16s | %14f | %2.12f | %14f | %14f |")

(defn mean [r]
  (-> r :mean first float))

(defn std-dev [r]
  (-> r :variance first float))

(defn lower-q [r]
  (-> r :lower-q first float))

(defn upper-q [r]
  (-> r :upper-q first float))

(defn overhead [r]
  (float (or (:overhead r) 0.0)))


(defn format-result
  [scales result]
  (println :scales scales)
  (let [label (if (:label result) (name (:label result)) "none")
        mean (* (mean result) (first (:mean scales)))
        _ (println :mean mean)
        std-dev (* (std-dev result) (first (:std-dev scales)))
        lower-q (* (lower-q result) (first (:lower-q scales)))
        upper-q (* (upper-q result) (first (:upper-q scales)))]
    (println (format result-line label mean std-dev lower-q upper-q))))

(defn header [scales]
  (let [labels (map (fn [[thing [_ unit]]]
                      (str (name thing)
                           (when (not= "" unit)
                             (str " (" unit ")"))))
                    scales)]
    (println (apply (partial format line) labels)))
  (println (str "|------------------+----------------+----------------+"
                "----------------+----------------|")))

(defmacro qbench [label f]
  `(fn [] (assoc (quick-benchmark (fn [] ~f) [[:os] [:runtime :verbose]])
            :label ~label)))

(defmacro bench [label f]
  `(fn [] (assoc (benchmark (fn [] ~f) [[:os] [:runtime :verbose]])
            :label ~label)))

(defn avg [nums]
  (float (/ (apply + nums) (count nums))))

(defn as-org-table [& fns]
  (let [results (for [f fns] (f))
        _ (println results)
        mean-h (scale-for (map mean results))
        std-dev-h (scale-for (map std-dev results))
        lower-q-h (scale-for (map lower-q results))
        upper-q-h (scale-for (map upper-q results))
        scales {:label [1 ""] :mean mean-h :std-dev [1e9 "ns"]
                :lower-q lower-q-h :upper-q upper-q-h}]
    (header scales)
    (dorun (map (partial format-result scales)
                results))))

(comment
  (as-org-table
   (qbench :thing (Thread/sleep 1000))
   (qbench :thing (Thread/sleep 5000))
   (qbench :thing (Thread/sleep 2000)))

  (as-org-table
   (qbench :label (dostuff))
   (bench :label2 (dootherstuff))
   (qbench :yep (domorethings)))

  ;; | label | mean | std-dev | lower q | upper q |
  ;; |-------+------+---------+---------+---------|
  ;; | etc                                        |

  )

#_
{:lower-q [1.0779520167285699E-8 (1.0779520167285699E-8 1.0786548817538096E-8)]
 :sample-count 6
 :tail-quantile 0.025
 :final-gc-time 84609000
 :mean [1.0972173423524247E-8 (1.0804723223854168E-8 1.1271771346515789E-8)]
 :total-time 0.603026
 :overhead nil
 :sample-variance [9.950045204099795E-20 (0.0 0.0)]
 :samples (98729000 106151000 101009000 98954000 98939000 99244000)
 :sample-mean [1.0973356238453452E-8 (1.0027045471121187E-8 1.1919667005785718E-8)]
 :outlier-variance 0.13888888888888865
 :warmup-executions 494450276
 :upper-q [1.1495609973291677E-8 (1.0831791488580233E-8 1.1589875773861218E-8)]
 :options {[:os] [:runtime :verbos]
           :max-gc-attempts 100
           :samples 6
           :target-execution-time 100000000
           :warmup-jit-period 5000000000
           :tail-quantile 0.025
           :bootstrap-size 500}
 :label :thing
 :variance [9.922717711701532E-20 (4.808199410982304E-22 1.892008660369913E-19)]
 :warmup-time 5601530000
 :execution-count 9158942}
