;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               coins.clj
;;;;
;;;;   Started:            Sun Mar  7 18:05:56 2021
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;

(ns abc.coins
  (:require [clojure.pprint :refer [cl-format]]))

(defn toss-coin []
  (case (rand-int 2)
    0 :head
    1 :tail))

;; (defn toss-coin []
;; (let [toss  (case (rand-int 2)
;;     0 :head
;;     1 :tail)]
;;   (println toss)
;;   toss))

(defn percentage [bin total]
  (cl-format false "~,2F%" (* 100.0 (/ bin total))))

;; (defn trial
;;   ([] (trial 100))
;;   ([count] (letfn [(toss [i heads tails heads-run max-heads-run tails-run max-tails-run]
;;                      (cond (zero? i) `(:heads ~heads (~(percentage heads count)) run ~(max heads-run max-heads-run)
;;                                        :tails ~tails (~(percentage tails count)) run ~(max tails-run max-tails-run))
;;                            :else (case (toss-coin)
;;                                    :head (toss (dec i) (inc heads) tails (inc heads-run) max-heads-run 0 (max tails-run max-tails-run))
;;                                    :tail (toss (dec i) heads (inc tails) 0 (max heads-run max-heads-run) (inc tails-run) max-tails-run))))]
;;              (toss count 0 0 0 0 0 0))))



;; ;;;
;; ;;;   Workshop ch. 5
;; ;;;   
;; (defn coin-runs [coins]
;;   (:result (reduce (fn [{:keys [run result]} coin]
;;                      (if (and (seq run)
;;                               (or (and (= (first run) coin :head))
;;                                   (and (= (first run) coin :tail))))
;;                        {:run (conj run coin)
;;                         :result (conj result [coin (count run)])}
;;                        {:run [coin]
;;                         :result (conj result [coin 0])}))
;;                    {:run [] :result []}
;;                    coins)))

(defn trial1
  ([] (trial1 100))
  ([n] (letfn [(stats
                 ([heads tails max-heads-run max-tails-run]
                  (stats heads tails max-heads-run max-tails-run 0))
                 ([heads tails max-heads-run max-tails-run max-alt-run]
                  {:heads [heads (list (percentage heads n)) 'run max-heads-run]
                   :tails [tails (list (percentage tails n)) 'run max-tails-run]
                   :alt max-alt-run}))
               (start [i]
                 (case (toss-coin)
                   :head (h0 (dec i))
                   :tail (t0 (dec i))))
               (h0 [i]
                 (cond (zero? i) (stats 1 0 1 0)
                       :else (case (toss-coin)
                               :head (hh (dec i) 2 0 2 0 0 0)
                               :tail (ht (dec i) 1 1 1 1 0 1 0 :alt-run-type :ht))))
               (t0 [i]
                 (cond (zero? i) (stats 0 1 0 1)
                       :else (case (toss-coin)
                               :head (th (dec i) 1 1 1 0 1 1 0 :alt-run-type :th)
                               :tail (tt (dec i) 0 2 0 2 0 0))))
               (hh [i heads tails heads-run max-heads-run max-tails-run max-alt-run]
                 (cond (zero? i) (stats heads tails (max heads-run max-heads-run) max-tails-run max-alt-run)
                       :else (case (toss-coin)
                               :head (hh (dec i) (inc heads) tails (inc heads-run) max-heads-run max-tails-run max-alt-run)
                               :tail (ht (dec i) heads (inc tails) (max heads-run max-heads-run) 1 max-tails-run 1 max-alt-run :alt-run-type :ht))))
               (ht [i heads tails max-heads-run tails-run max-tails-run alt-run max-alt-run & {:keys [alt-run-type]}]
                 (cond (zero? i) (stats heads tails max-heads-run (max tails-run max-tails-run) (max alt-run max-alt-run))
                       :else (case (toss-coin)
                               :head (th (dec i) (inc heads) tails 1 max-heads-run (max tails-run max-tails-run)
                                         (case alt-run-type
                                           :th (inc alt-run)
                                           :ht alt-run)
                                         max-alt-run
                                         :alt-run-type alt-run-type)
                               :tail (tt (dec i) heads (inc tails) max-heads-run (inc tails-run) max-tails-run (max alt-run max-alt-run)))) )
               (th [i heads tails heads-run max-heads-run max-tails-run alt-run max-alt-run & {:keys [alt-run-type]}]
                 (cond (zero? i) (stats heads tails (max heads-run max-heads-run) max-tails-run (max alt-run max-alt-run))
                       :else (case (toss-coin)
                               :head (hh (dec i) (inc heads) tails (inc heads-run) max-heads-run max-tails-run (max alt-run max-alt-run))
                               :tail (ht (dec i) heads (inc tails) (max heads-run max-heads-run) 1 max-tails-run
                                         (case alt-run-type
                                           :ht (inc alt-run)
                                           :th alt-run)
                                         max-alt-run
                                         :alt-run-type alt-run-type))))
               (tt [i heads tails max-heads-run tails-run max-tails-run max-alt-run]
                 (cond (zero? i) (stats heads tails max-heads-run (max tails-run max-tails-run) max-alt-run)
                       :else (case (toss-coin)
                               :head (th (dec i) (inc heads) tails 1 max-heads-run (max tails-run max-tails-run) 1 max-alt-run :alt-run-type :th)
                               :tail (tt (dec i) heads (inc tails) max-heads-run (inc tails-run) max-tails-run max-alt-run))))]
         (start n))))

(defn trial2
  ([] (trial2 100))
  ([n] (letfn [(stats
                 ([results]
                  (stats results 0))
                 ([{:keys [heads tails max-heads-run max-tails-run]} max-alt-run]
                  {:heads [heads (list (percentage heads n)) 'run max-heads-run]
                   :tails [tails (list (percentage tails n)) 'run max-tails-run]
                   :alt max-alt-run}))
               (start [i results]
                 (case (toss-coin)
                   :head (h0 (dec i) (merge results {:heads 1 :max-heads-run 1}))
                   :tail (t0 (dec i) (merge results {:tails 1 :max-tails-run 1}))))
               (h0 [i results]
                 (cond (zero? i) (stats results)
                       :else (case (toss-coin)
                               :head (hh (dec i) (merge results {:heads 2 :heads-run 2}))
                               :tail (ht (dec i)
                                         (merge results {:heads 1 :tails 1 :max-heads-run 1 :tails-run 1 :alt-run 1})
                                         :alt-run-type :ht))))
               (t0 [i results]
                 (cond (zero? i) (stats results)
                       :else (case (toss-coin)
                               :head (th (dec i)
                                         (merge results {:heads 1 :tails 1 :heads-run 1 :max-tails-run 1 :alt-run 1})
                                         :alt-run-type :th)
                               :tail (tt (dec i) (merge results {:tails 2 :tails-run 2})))) )
               (hh [i {:keys [heads tails heads-run max-heads-run max-tails-run max-alt-run] :as results}]
                 (cond (zero? i) (stats (merge results {:max-heads-run (max heads-run max-heads-run)}) max-alt-run)
                       :else (case (toss-coin)
                               :head (hh (dec i) (merge results {:heads (inc heads) :heads-run (inc heads-run)}))
                               :tail (ht (dec i)
                                         (merge results {:tails (inc tails)
                                                         :max-heads-run (max heads-run max-heads-run)
                                                         :tails-run 1
                                                         :alt-run 1})
                                         :alt-run-type :ht))))
               (ht [i {:keys [heads tails max-heads-run tails-run max-tails-run alt-run max-alt-run] :as results} & {:keys [alt-run-type]}]
                 (cond (zero? i) (stats (merge results {:max-tails-run (max tails-run max-tails-run)}) (max alt-run max-alt-run))
                       :else (case (toss-coin)
                               :head (th (dec i) (merge results {:heads (inc heads) :heads-run 1 :max-tails-run (max tails-run max-tails-run)
                                                                 :alt-run (case alt-run-type
                                                                            :th (inc alt-run)
                                                                            :ht alt-run)})
                                         :alt-run-type alt-run-type)
                               :tail (tt (dec i) (merge results {:tails (inc tails) :tails-run (inc tails-run) :max-alt-run (max alt-run max-alt-run)})))) )
               (th [i {:keys [heads tails heads-run max-heads-run max-tails-run alt-run max-alt-run] :as results} & {:keys [alt-run-type]}]
                 (cond (zero? i) (stats (merge results {:max-heads-run (max heads-run max-heads-run)}) (max alt-run max-alt-run))
                       :else (case (toss-coin)
                               :head (hh (dec i) (merge results {:heads (inc heads) :heads-run (inc heads-run) :max-alt-run (max alt-run max-alt-run)}))
                               :tail (ht (dec i) (merge results {:tails (inc tails) :max-heads-run (max heads-run max-heads-run) :tails-run 1
                                                                 :alt-run (case alt-run-type
                                                                            :ht (inc alt-run)
                                                                            :th alt-run)})
                                         :alt-run-type alt-run-type))))
               (tt [i {:keys [heads tails max-heads-run tails-run max-tails-run max-alt-run] :as results}]
                 (cond (zero? i) (stats (merge results {:max-tails-run (max tails-run max-tails-run)}) max-alt-run)
                       :else (case (toss-coin)
                               :head (th (dec i) (merge results {:heads (inc heads)
                                                                 :heads-run 1
                                                                 :max-tails-run (max tails-run max-tails-run)
                                                                 :alt-run 1})
                                         :alt-run-type :th)
                               :tail (tt (dec i) (merge results {:tails (inc tails) :tails-run (inc tails-run)})))) )]
         (start n {:heads 0
                   :tails 0
                   :heads-run 0
                   :tails-run 0
                   :max-heads-run 0
                   :max-tails-run 0
                   :max-alt-run 0}))))

(defn trial3
  ([] (trial3 100))
  ([n] (letfn [(stats
                 ([results]
                  (stats results 0))
                 ([{:keys [heads tails max-heads-run max-tails-run]} max-alt-run]
                  {:heads [heads (list (percentage heads n)) 'run max-heads-run]
                   :tails [tails (list (percentage tails n)) 'run max-tails-run]
                   :alt max-alt-run}))
               (start [i results]
                 (case (toss-coin)
                   :head #(h0 (dec i) (merge results {:heads 1 :max-heads-run 1}))
                   :tail #(t0 (dec i) (merge results {:tails 1 :max-tails-run 1}))))
               (h0 [i results]
                 (cond (zero? i) (stats results)
                       :else (case (toss-coin)
                               :head #(hh (dec i) (merge results {:heads 2 :heads-run 2}))
                               :tail #(ht (dec i)
                                          (merge results {:heads 1 :tails 1 :max-heads-run 1 :tails-run 1 :alt-run 1})
                                          :alt-run-type :ht))))
               (t0 [i results]
                 (cond (zero? i) (stats results)
                       :else (case (toss-coin)
                               :head #(th (dec i)
                                          (merge results {:heads 1 :tails 1 :heads-run 1 :max-tails-run 1 :alt-run 1})
                                          :alt-run-type :th)
                               :tail #(tt (dec i) (merge results {:tails 2 :tails-run 2})))) )
               (hh [i {:keys [heads tails heads-run max-heads-run max-alt-run] :as results}]
                 (cond (zero? i) (stats (merge results {:max-heads-run (max heads-run max-heads-run)}) max-alt-run)
                       :else (case (toss-coin)
                               :head #(hh (dec i) (merge results {:heads (inc heads) :heads-run (inc heads-run)}))
                               :tail #(ht (dec i)
                                          (merge results {:tails (inc tails)
                                                          :max-heads-run (max heads-run max-heads-run)
                                                          :tails-run 1
                                                          :alt-run 1})
                                          :alt-run-type :ht))))
               (ht [i {:keys [heads tails tails-run max-tails-run alt-run max-alt-run] :as results} & {:keys [alt-run-type]}]
                 (cond (zero? i) (stats (merge results {:max-tails-run (max tails-run max-tails-run)}) (max alt-run max-alt-run))
                       :else (case (toss-coin)
                               :head #(th (dec i) (merge results {:heads (inc heads) :heads-run 1 :max-tails-run (max tails-run max-tails-run)
                                                                  :alt-run (case alt-run-type
                                                                             :th (inc alt-run)
                                                                             :ht alt-run)})
                                          :alt-run-type alt-run-type)
                               :tail #(tt (dec i) (merge results {:tails (inc tails) :tails-run (inc tails-run) :max-alt-run (max alt-run max-alt-run)})))) )
               (th [i {:keys [heads tails heads-run max-heads-run alt-run max-alt-run] :as results} & {:keys [alt-run-type]}]
                 (cond (zero? i) (stats (merge results {:max-heads-run (max heads-run max-heads-run)}) (max alt-run max-alt-run))
                       :else (case (toss-coin)
                               :head #(hh (dec i) (merge results {:heads (inc heads) :heads-run (inc heads-run) :max-alt-run (max alt-run max-alt-run)}))
                               :tail #(ht (dec i) (merge results {:tails (inc tails) :max-heads-run (max heads-run max-heads-run) :tails-run 1
                                                                  :alt-run (case alt-run-type
                                                                             :ht (inc alt-run)
                                                                             :th alt-run)})
                                          :alt-run-type alt-run-type))))
               (tt [i {:keys [heads tails tails-run max-tails-run max-alt-run] :as results}]
                 (cond (zero? i) (stats (merge results {:max-tails-run (max tails-run max-tails-run)}) max-alt-run)
                       :else (case (toss-coin)
                               :head #(th (dec i) (merge results {:heads (inc heads)
                                                                  :heads-run 1
                                                                  :max-tails-run (max tails-run max-tails-run)
                                                                  :alt-run 1})
                                          :alt-run-type :th)
                               :tail #(tt (dec i) (merge results {:tails (inc tails) :tails-run (inc tails-run)})))) )]
         (trampoline start n {:heads 0
                              :tails 0
                              :heads-run 0
                              :tails-run 0
                              :max-heads-run 0
                              :max-tails-run 0
                              :max-alt-run 0}))))
