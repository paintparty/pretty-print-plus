(ns pretty-print-plus.util
  (:require [clojure.string :as string]))


(defn stringify [v] (if (string? v) (str "\"" v "\"") (str v)))

(def type-map
  (reduce-kv #(assoc %1 (type %3) (name %2))
             {}
             {:String "s"
              :PersistentVector []
              :PersistentArrayMap {}
              :PersistentHashSet #{}
              :LazySeq (lazy-seq nil)
              :Nil nil
              :Var #'type-map
              :Number 1
              :Keyword :kw
              :Symbol 'sym
              :Boolean true
              :Function #(do nil)
              :RegExp #"\d"}))

(defn type-as-string [v]
  (-> v type type-map))

(def reduce-indexed reduce-kv)

(defn ccons [& args]
  (let [items (-> args drop-last reverse)
        coll  (last args)]
    (reduce #(cons %2 %1) coll items)))

(defn pf-coll
  ([pred coll acc pathvec]
    (reduce-indexed
     (fn pf-coll-inner [acc idx v]
       (if (or (pred idx v) (pred v))
         (conj acc (conj pathvec idx))
         (if (coll? v)
           (pf-coll pred v acc (conj pathvec idx))
           acc)))
     acc
     coll))
  ([pred coll]
    (pf-coll pred coll [] [])))


(defn update-vals-inner [coll acc pathvec pred-fns]
    (reduce-indexed
     (fn [acc idx v]
       (let [trans-val-fn (fn [idx v] (some #(% idx v) pred-fns))]
         (if-let [trans-val (trans-val-fn idx v)]
           (update-in acc (conj pathvec idx) #(do trans-val))
           (if (coll? v)
             (update-vals-inner v acc (conj pathvec idx) pred-fns)
             acc))))
     acc
     coll))


(defn update-vals-quoted [qcoll coll & pred-fns]
  "qcoll is a the coll quoted and the coll is passed to update-vals-inner
   as the accumulator. Meant to be called from a macro."
  (update-vals-inner qcoll coll [] pred-fns))


(defn update-vals [coll & fns]
  "'Updates' any value in nested data structure with the result of
   passing said value (and optionally the corresponding index or key)
   to the first function that returns logical true.
   Supplied functions should accept a single argument, (fn [v] ...),
   or two arguments (fn [idx-or-key v] ...)"
  (update-vals-inner coll coll [] fns))



;;;;; counting chars in colls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn coll-spaces [coll]
  (if (map? coll)
    (let [nkeys (count (keys coll))] (+ nkeys (dec nkeys)))
    (-> coll count dec)))

(defn num-braces-spaces-tags [coll]
  (+ (coll-spaces coll)
     2
     (when (set? coll) 1)))

(declare coll-char-count)

(defn count-chars [v]
  (if (coll? v)
    (coll-char-count v)
    (-> v stringify count)))

(defn coll-char-count [coll]
  (let [num-braces-spaces-tags (num-braces-spaces-tags coll)
        f (fn [acc k v]
            (+ acc
               (when (map? coll) (count-chars k))
               (count-chars v)))
        num-chars-inside-coll (reduce-kv f 0 coll)
        ret (+ num-chars-inside-coll num-braces-spaces-tags)]
    ret))


;;;;;; diccup span utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-diccup-tag? [v]
  (and
   (keyword? v)
   (not (nil? (re-find #"^(span|line)(?:\..+)?$" (name v))))))

(defn diccup-span-tag? [v]
  (and
   (keyword? v)
   (not (nil? (re-find #"^(span)(?:\..+)?$" (name v))))))

(defn diccup-span? [coll]
  (and (vector? coll)
       (diccup-span-tag? (first coll))))

(defn diccup-line-or-span? [coll]
  (and (vector? coll)
       (is-diccup-tag? (first coll))))

(defn diccup-coll-wrap? [v]
  (and (vector? v)
       (contains? #{:dc/span.PersistentArrayMap
                    :dc/span.PersistentHashSet
                    :dc/span.PersistentVector
                    :dc/span.LazySeq}
                  (first v))))

;;spec This
(defn dc-line-vec? [v] (and (vector? v) (= (first v) :dc/line)))

(defn coll-or-wrapped-coll [v]
  "Examples of input that would return true:
   [:dc/span {:a 1 :b 2}]
   [:dc/span [1 2 3 4]]
   {:a 1 :b 2}
   [1 2 3 4]

   Examples of input that would return nil:
   [:dc/line {:a 1 :b 2}]
   [:dc/span 42]
   [:dc/span [:dc/span {:a 1 :b 2}]]
   42"
  (if (diccup-span? v)
    (let [sv (second v)]
      (when (and (= (count v) 2)
                 (coll? sv)
                 (not (diccup-line-or-span? sv)))
        sv))
    (when (coll? v) v)))

(defn isolated-coll-line? [v]
  (if (and (dc-line-vec? v)
           (coll-or-wrapped-coll (second v)))
    true
    false))

(defn isolated-coll-from-line [v]
  (when (dc-line-vec? v)
    (coll-or-wrapped-coll (second v))))

(defn reserve-diccup-tag
  ([idx v]
    (when (is-diccup-tag? v)
      (-> v
          name
          (string/replace #"^(span|line)" #(str "dc/" (second %)))
          keyword)))
  ([v] (reserve-diccup-tag nil v)))

(defn reserve-user-string
  ([idx v] ;(log reserve-user-string)
           ;(log "idx => " idx)
           ;(log "v => " v)
           (when (and (string? v) (not (keyword? idx)))
             (str "__DCML_STRING__" v)))
  ([v] (reserve-user-string nil v)))

(defn find-user-string [v]
  (re-find #"^__DCML_STRING__(.+)" v))

(defn user-string? [v] (when (string? v) (find-user-string v)))

(defn dehydrated-user-string [v]
  (when (user-string? v)
    (second (find-user-string v))))

(defn og-user-string [v]
  (if-let [og-user-string (dehydrated-user-string v)]
    og-user-string
    v))

(defn dquote-string [v] (str "\"" v "\""))

(defn dcml-kw? [v]
  (and
   (keyword? v)
   (= "dc" (namespace v))
   (re-find #"^(?:span|line)(?:\..+)?$" (name v))))

(defn isolate-colls [pred ln-vecs]
  (let [idxs (map drop-last (pf-coll pred ln-vecs))
        nwl-span [:dc/span.isolate-pam-nwl "\n"]
        update-fn #(do [:dc/span.isolate-pam nwl-span % nwl-span])
        f (fn [ln-vecs path] (update-in ln-vecs path update-fn))]
    (reduce f ln-vecs idxs)))


(defn rainbow-color [depth-idx]
  (let [rkclrs [:blood :purps :skye :grass :gold]
        c-idx (mod depth-idx (count rkclrs))
        rkclr (nth rkclrs c-idx)]
    rkclr))


(defn rainbow-bg-color [depth-idx]
  (let [
        rkclrs* [:bgc-faint-blood-light
                :bgc-faint-purps-light
                :bgc-faint-skye-medium
                :bgc-faint-grass-dark
                :bgc-faint-gold-dark
                :bgc-faint-blood-dark
                :bgc-faint-purps-dark
                :bgc-faint-skye-medium
                :bgc-faint-grass-light
                :bgc-faint-gold-light
                ]
        rkclrs [:bgc-faint-blood-light
                :bgc-faint-purps-light
                :bgc-faint-skye-medium
                :bgc-faint-grass-light
                :bgc-faint-gold-light
                ]
        c-idx (mod depth-idx (count rkclrs))
        rkclr (nth rkclrs c-idx)]
    rkclr))

(defn circ-range [seq]
  (concat seq (-> (drop 1 seq) drop-last reverse)))

(defn step-range
  "Returns a lazy seq of n numbers from start (inclusive) to end
  (inclusive). Step amount is derived from inputs. Start and
  end cannot be equal. n must be a natural integer."
  [start end n]
  (let [step-amt (/ (- end start) (dec n))
        new-end (if (< start end) (inc end) (dec end))]
    (range start new-end step-amt)))

(defn step-range-ex
  "Returns a lazy seq of n numbers from start (inclusive) to end
  (exclusive). Step amount is derived from inputs. Start and
  end cannot be equal. n must be a natural integer."
  [start end n]
  (range start end (/ end n)))



;;; css style utils .....................................................
(defn map->css [m]
  (reduce-kv #(str %1 (name %2) ":" %3 ";") "" m))

(defn merged-style-map
  "Given a style-map of style-blocks, a collection of kws that
   correspond to keys in said style-map, and an (optional), inline
   style block, returns a merged style block.
   Example:

   (def style-map {:error {:color \"red\"}
                   :pam-td {:color \"blue\" :margin-left \"2px\"}})

   (merged-style-map style-map
                     [:pam-td :error]
                     {:background-color \"yellow\"})

   => {:color \"red\" :margin-left \"2px\" :background-color \"yellow\"}"
  ([style-map kws inline-style]
    (let [style-maps (mapv #(% style-map) kws)]
      (apply merge (conj style-maps inline-style))))
  ([style-map kws]
    (merged-style-map kws nil)))
