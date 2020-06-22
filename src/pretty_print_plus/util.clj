(ns pretty-print-plus.util)

(defmacro kv->
  "m must be a map of 0 or more key-value pairs. pipeline must be a vector
   with an even number of elements. These elements must be keyword / function
   pairs. The first keyword in the pipeline vector is assoc'd to m with the result of passing
   m to the inital function in pipeline(which is the second element). Ths process is repeated with the
   resulting map until pipeline is exhausted. The resulting map is bound to
   name, which is passed through each successive form in the manner of the
   as-> macro"
  [m name pipeline & forms]
  `(let [fpl# (interleave (map keyword (quote ~pipeline)) ~pipeline)
         pl# (if (every? fn? ~pipeline) fpl# ~pipeline)
         pipeline# (apply array-map pl#)
         f# (fn [m# k# v#]
              (let [result# (v# m#)
                    keys-vec?# (and (map? result#) (vector? k#))
                    keys-map?# (and (map? result#) (map? k#))]
                (cond
                  keys-vec?# (merge m# result#)
                  keys-map?# (reduce-kv #(assoc %1 %2 (%3 result#)) m# k#)
                  :else (assoc m# k# result#))))
         ~name (reduce-kv f# ~m pipeline# )
         ~@(interleave (repeat name) (butlast forms))]
     ~(if (empty? forms)
        name
        (last forms))))
