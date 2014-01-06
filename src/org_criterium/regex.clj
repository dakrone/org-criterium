(ns org-criterium.regex
  "The terrible, regex-y version."
  (:require [criterium.core :as bench]))

(defmacro qbench [label f]
  `(fn []
     {:text (with-out-str (bench/quick-bench (fn [] ~f)))
      :label ~label}))

(defmacro bench [label f]
  `(fn [] (assoc (bench/benchmark (fn [] ~f) [[:os] [:runtime :verbose]])
            :label ~label)))

;; Evaluation count : 6 in 6 samples of 1 calls.
;;              Execution time mean : 1.000992 sec
;;     Execution time std-deviation : 273.396415 µs
;;    Execution time lower quantile : 1.000435 sec ( 2.5%)
;;    Execution time upper quantile : 1.001108 sec (97.5%)
;;                    Overhead used : 9.744289 ns
;;
;; Found 1 outliers in 6 samples (16.6667 %)
;;  low-severe   1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers

(def ex
  (str "Evaluation count : 6 in 6 samples of 1 calls.
                    Execution time mean : 1.000992 sec
           Execution time std-deviation : 273.396415 µs
          Execution time lower quantile : 1.000435 sec ( 2.5%)
          Execution time upper quantile : 1.001108 sec (97.5%)
                          Overhead used : 9.744289 ns

       Found 1 outliers in 6 samples (16.6667 %)
         low-severe   1 (16.6667 %)
        Variance from outliers : 13.8889 % Variance is moderately inflated by outliers"))

(defn parse-criterium-output
  [text]
  (let [r-mean #"Execution time mean : (.+) (.+)\n"
        r-std-dev #"Execution time std-deviation : (.+) (.+)\n"
        r-l-quant #"Execution time lower quantile : (.+) (.+) \(.+\)\n"
        r-u-quant #"Execution time upper quantile : (.+) (.+) \(.+\)\n"
        [_ mean mean-unit] (re-find r-mean text)
        [_ std-dev std-dev-unit] (re-find r-std-dev text)
        [_ l-quant l-quant-unit] (re-find r-l-quant text)
        [_ u-quant u-quant-unit] (re-find r-u-quant text)]
    {:mean [mean mean-unit]
     :std-dev [std-dev std-dev-unit]
     :l-quant [l-quant l-quant-unit]
     :u-quant [u-quant u-quant-unit]}))

(defn as-org-table [& fns]
  (let [results (for [f fns] (f))
        _ (println results)]
    #_(header scales)
    #_(dorun (map (partial format-result scales)
                  results))))
