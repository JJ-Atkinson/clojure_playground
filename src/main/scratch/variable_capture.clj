(ns scratch.variable-capture
  (:require [meander.epsilon :as m]
            [meander.strategy.epsilon :as r]
            [clojure.uuid :as cuuid])
  (:import (java.util UUID)))


(defn uuid []
  (UUID/randomUUID)
  nil)


(defn capture [form meta]
  `(capturei ~meta ~form))


(def capture-atom (atom {}))
(defn capturei [meta result]
  (swap! capture-atom assoc (:path meta) 
    (assoc meta :value result))
  result)

(defn path-extend [meta path-extension]
  (update meta :path #(vec (concat %
                             (if (vector? path-extension)
                               path-extension [path-extension])))))

(defmulti writeback-syntax "Based on the form type, writes out the code again." (fn [type & _] type))

(defmethod writeback-syntax :let [_ {:keys [var-symbols var-expressions
                                            out-expressions meta
                                            recurse-target]}]
  (let [index|symbol|expression (map vector (range) var-symbols var-expressions)
        bindings (->> index|symbol|expression
                   (mapcat (fn [[i s e]]
                             (let [localized-meta (-> meta
                                                    (path-extend [1 (inc (* i 2))])
                                                    (assoc :bound-to s))]
                               [s (capture (recurse-target e localized-meta) localized-meta)])))
                   vec)
        out-expressions (->> out-expressions
                          (map-indexed (fn [idx e]
                                         (let [localized-meta (path-extend meta [(+ idx 2)])]
                                           (capture (recurse-target e localized-meta) localized-meta)))))]
    (concat `(let ~bindings) out-expressions)))

(defmethod writeback-syntax :fn-invoke [_ {:keys [meta f args recurse-target]}]
  (let [args (->> args
               (map-indexed
                 (fn [i e]
                   (let [localized-meta (path-extend meta [(inc i)])]
                     (capture (recurse-target e localized-meta) localized-meta)))))]
    (concat `(~f) args)))

(defmethod writeback-syntax :simple-symbol [_ {:keys [meta symbol]}]
  (capture symbol meta))

(do

  (defn rewritec
    ([code] (rewritec code {:path []}))
    ([code meta]
     (m/match code
       (let [!var !val ...]
         .
         !outs ...)
       (writeback-syntax :let
         {:var-symbols     !var
          :var-expressions !val
          :out-expressions !outs
          :meta            meta
          :recurse-target  rewritec})

       (?function-or-macro . !args ...)
       (writeback-syntax :fn-invoke
         {:meta           meta
          :f              ?function-or-macro
          :args           !args
          :recurse-target rewritec})

       (m/pred symbol? ?x)
       (writeback-syntax :simple-symbol
         {:meta           meta
          :symbol ?x})

       ?x ?x)))

  (rewritec '(let [b 2
                   c b
                   a (partial + b 4)]
               (a 3)
               b)))

(comment (clojure.core/let
           [b
            (scratch.variable-capture/capturei {:path [1 1], :bound-to 'b} 2)
            c
            (scratch.variable-capture/capturei
              {:path [1 3], :bound-to 'c}
              (scratch.variable-capture/capturei {:path [1 3], :bound-to 'c} b))
            a
            (scratch.variable-capture/capturei
              {:path [1 5], :bound-to 'a}
              (partial
                (scratch.variable-capture/capturei
                  {:path [1 5 1], :bound-to 'a}
                  (scratch.variable-capture/capturei {:path [1 5 1], :bound-to 'a} +))
                (scratch.variable-capture/capturei
                  {:path [1 5 2], :bound-to 'a}
                  (scratch.variable-capture/capturei {:path [1 5 2], :bound-to 'a} b))
                (scratch.variable-capture/capturei {:path [1 5 3], :bound-to 'a} 4)))]
           (scratch.variable-capture/capturei {:path [2]} (a (scratch.variable-capture/capturei {:path [2 1]} 3)))
           (scratch.variable-capture/capturei {:path [3]} (scratch.variable-capture/capturei {:path [3]} b))))