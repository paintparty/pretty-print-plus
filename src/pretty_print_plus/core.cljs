(ns pretty-print-plus.core
  (:require
   [clojure.string :as string]
   [clojure.pprint :refer [pprint]]
   [clojure.walk :as w]
   [pretty-print-plus.util :as u :include-macros true]
   [pretty-print-plus.style :as style]
   [pretty-print-plus.colors :as colors :refer [nth-hsla]]))


;; toggle style here
(def dcp-style :line)
(def theme colors/cljs-devtools-hsl)
(def max-depth 20)

;;; PersistentArrayMap formatting
(declare format-pam)

;;; Vector and list formatting
(declare format-seq)

;;; set formatting
(declare format-set)

(defn quoted-string [s]
  (str \" s \"))

(defn syntax-color [k]
  (when theme
    (apply colors/hsla-string (concat (vals (k theme)) [1]))))

(defn format-val
  ([v opts key?]
   (cond

     (instance? cljs.core/Atom v)
     [:span
      [:span {:style {:line-height "1.1rem"
                      :color "hsla(200, 100%, 30%, 1)" #_(syntax-color :atom)
                      :vertical-align "middle"
                      :padding "0px 1px"
                      :font-size "9px"}} "@"]
      [:span.Atom
       {:style {:background-color (syntax-color :atom)
                :background-image "linear-gradient(to bottom right, #91dc47, #5881d8)"
                :font-size "10px"
                :font-weight "bold"
                :color "white"
                :border-radius "2px"
                :padding "1px 5px"
                :margin-right "2px"
                :line-height "1.1rem"
                :vertical-align "middle"}} "Atom"]
      [:span {:style {:line-height "1.1rem"
                      :color "hsla(200, 100%, 30%, 1)" #_(syntax-color :atom)
                      :padding "0 1px"
                      :vertical-align "middle"
                      :font-size "9px"}} "=>"]
      (let [d @v]
        (format-val d opts))]

     (fn? v)
     (let [fn-name+args (second (re-find #"^function (.*)\{" (str v)))
           [fq-name args] (string/split fn-name+args #"\(")
           args-vec (->> args drop-last (remove #(= % ",")) (string/join " "))
           ns+fn-name (string/split fq-name #"\$")
           nm (str (string/join "." (drop-last ns+fn-name)) "/" (last ns+fn-name))]
       [:span
        [:span.Fn {:style {:background-color "#5cb382"
                           :font-size "10px"
                           :font-weight "bold"
                           :color "white"
                           :border-radius "2px"
                           :padding "1px 5px"
                           :line-height "1.1rem"
                           :vertical-align "middle"}} "fn"]
        [:span
         {:style {:color (syntax-color :fn)
                  :padding "0 3px"}}
         nm]
        [:span.arglist
         {:style {:color (syntax-color :fn-args)}}
         (str "[" args-vec "]")]])
     (string? v) [:span.String {:style {:color (syntax-color :string)}} (quoted-string v)]
     (keyword? v)
     (if key?
       [:span ":" [:span.Keyword {:style {:color (syntax-color :keyword)}} (str  (name v))]]
       [:span.Keyword {:style {:color (syntax-color :keyword)}} (str  v)])
     (number? v) [:span.Number {:style {:color (syntax-color :integer)}} (str  v)]
     (boolean? v) [:span.Boolean {:style {:color (syntax-color :bool)}} v]
     (nil? v) [:span.Nil {:style {:color (syntax-color :nil)}} "nil"]
     (vector? v) (format-pam v (update-in opts [:depth] inc))
     (list? v) (format-pam v (update-in opts [:depth] inc))
     (set? v) (format-pam v (update-in opts [:depth] inc))
     (map? v) (do
                [:span
                 (when-not (or (= cljs.core/PersistentArrayMap (type v))
                               (= cljs.core/PersistentHashMap (type v)))
                   [:span
                    {:style {:whitespace "nowrap"}}
                    [:span {:style {:line-height "1.1rem"
                                    :color "hsla(200, 100%, 30%, 1)" #_(syntax-color :atom)
                                    :vertical-align "middle"
                                    :padding "0px 1px"
                                    :font-size "10px"}} ""]
                    [:span.Atom
                     {:style {:background-color (syntax-color :atom)
                              :font-size "10px"
                              :font-weight "bold"
                              :color "white"
                              :border-radius "2px"
                              :padding "1px 5px"
                              :margin-right "2px"
                              :line-height "1.1rem"
                              :vertical-align "middle"}} (string/trim (with-out-str (pprint (type v))))]
                    [:span {:style {:line-height "1.1rem"
                                    :color "hsla(200, 100%, 30%, 1)" #_(syntax-color :atom)
                                    :padding "0 1px"
                                    :vertical-align "middle"
                                    :font-size "10px"}} "=>"]])
                 (format-pam v (update-in opts [:depth] inc))])
     :else v))
  ([v opts]
   (format-val v opts false)))


(defn truncate-map
  ([m opts]
    (let [depth (:depth opts)
          bg-color (style/nth-color depth style/rk-bg-colors)
          color (style/nth-color depth style/rk-colors)]
      [:span {:style (str "color:" color ";"
                          "background-color:" bg-color)}
                     "{" [:span {:style "color:gray" } "..." ] "}"]))
  ([m] (truncate-map m nil)))


(defn contains-complex-map? [pam]
  (some #(when (map? %)
           (some (fn [v] (when (map? v)
                           (< 1 (count v))))
                 (vals %)))
        pam))


(defn contains-coll-gt-10? [pam]
  (some #(and (coll? %) (< 10 (count %))) pam))


(defn map->table-fn [{:keys [opts coll-size strlen pam]}]
  (fn [idx v]
    (cond

      ;maps
      (map? pam)
      (let [empty-map? (empty? pam)
            key        (first v)
            val        (second v)
            ;; hsla-opts  (assoc opts :from :map->table.map)
            ;; fg-color   (nth-hsla opts)
            fg-color     (nth-hsla (:hsla-seq opts) (:depth opts))
            obr-str    (if (= idx 0) "{" "")
            cbr-str    (if (or empty-map? (= idx (dec coll-size))) "}" "")
            ;; key-str    (if (string? key) (quoted-string key) (str key))
            key-str    (format-val key opts true)
            val-str    (format-val val opts)
            style      {:style {:color fg-color}}
            ]
        (if empty-map?
          [:tr
           [:td.pam-td style [:span.pam-obr obr-str]]
           [:td.pam-td.pam-cbr style cbr-str]]
          [:tr
           [:td.pam-td style [:span.pam-obr obr-str]]
           [:td.pam-td.pam-key style key-str #_[:span.pam-key-str key-str]]
           [:td.pam-td.pam-val {:style {}} val-str]
           [:td.pam-td.pam-cbr style cbr-str]]))

      ;lists, vectors, sets
      (or (list? pam) (vector? pam) (set? pam))
      (let [val          v
            ;; hsla-opts    (assoc opts :from :map->table.vec)
            ;; fg-color     (nth-hsla opts)
            fg-color     (nth-hsla (:hsla-seq opts) (:depth opts))
            obr-str      (if (= idx 0)
                           (cond (vector? pam) "["
                                 (list? pam) "'("
                                 (set? pam) "#{")
                           " ")
            cbr-str      (if (or (empty? pam) (= idx (dec coll-size)))
                           (cond (vector? pam) "]"
                                 (list? pam) ")"
                                 (set? pam) "}")
                           "")
            val-str      (format-val val opts)
            single-line? (and (not (contains-complex-map? pam))
                              (not (contains-coll-gt-10? pam))
                              (< strlen 50)
                              (< (count pam) 10))
            va           (if single-line? "middle" "top")
            style        {:style {:color fg-color :vertical-align va}}
            cbr-style    {:style {:color fg-color :vertical-align (if single-line? "middle" "bottom")}}]
        (if (empty? pam)

         [:td
          [:td.pam-td style [:span.pam-obr.empty-obr obr-str]]
          [:td.pam-td.pam-cbr.empty-cbr cbr-style cbr-str]]

         [(if single-line? :td :tr)
             style
             [:td.pam-td style [:span.pam-obr obr-str]]
           ; [:td.pam-td.pam-key style [:span.pam-key-str key-str]]
             [:td.pam-td.seq-val val-str]
             [:td.pam-td.pam-cbr cbr-style cbr-str]])))))

(defn rainbow-indents
  [{:keys [guide-width indent-color bg-color]}]
  (let [indents+bg true
        rainbow-indent-as-bgi (style/linear-gradient
                               :to-right
                               indent-color
                               [indent-color guide-width]
                               [:transparent guide-width])
        gradient-bgi (style/linear-gradient :to-bottom-right bg-color :white)
        rainbow-indents-w-backgrounds (str rainbow-indent-as-bgi ", " gradient-bgi)
        just-rainbow-indents rainbow-indent-as-bgi ]
    {:style {
             ;:background-image just-rainbow-indents
             :background-image rainbow-indents-w-backgrounds
             :background-repeat "no-repeat, no-repeat"
             :background-size "100% calc(100% - 22px), 100% 100%"
             :background-position "3px 17px, 3px 0px"
            ;;  :border-bottom (str "2px solid " indent-color)
             }}))


(defn pam-table-bg-img [bg-color indent-color]
  ;put dcp-style in opts
  (if (= dcp-style :line)
    (rainbow-indents {:guide-width 2 :indent-color indent-color :bg-color bg-color})
    {:style {:background-image (str "linear-gradient(to bottom right, " bg-color ", white)")
             }}))

(defn pam-tbl
  [{:keys [depth
           pam
           opts
           map->table-fn
           bg-color
           indent-color]}]
  (if (> depth max-depth)
    (truncate-map pam opts)
    (vec (concat [:table.pam-table
                  (pam-table-bg-img bg-color indent-color)]
                 (if (empty? pam)
                   [(map->table-fn 0 pam)]
                   (map-indexed map->table-fn pam))))))


(defn coll-size [m] (-> m :pam count))

(defn strlen [m] (-> m :pam str count))

(defn format-pam
  ([pam opts]
    (let [{:keys [hsla-seq depth]} opts
          bg-alpha      (if (= dcp-style :line) 1 0.05)

          ; Make user-defined and add "staggered" option
          color-theme :rainbow-classic
          reverse-colors? false ;; Broken! not syncing with brackets
          transparent-bg? true
          grayscale-bg? true

          indent-color-seq* (get colors/indent-colors :rainbow-classic)
          indent-color-seq (if reverse-colors? indent-color-seq* (reverse indent-color-seq*) )
          indent-color  (nth-hsla indent-color-seq depth bg-alpha)
          bg-color-seq* (if transparent-bg?
                          '({:h 0 :s 0 :l 100})
                          (get colors/bg-colors (if grayscale-bg? :neutral color-theme)))
          bg-color-seq (if (and (not grayscale-bg?) (not transparent-bg?) reverse-colors?)
                         (reverse bg-color-seq*)
                         bg-color-seq*)
          bg-color      (nth-hsla bg-color-seq depth 1)
          strlen        (-> pam str count)
          coll-size     (count pam)
          map->table-fn (map->table-fn {:opts opts
                                        :coll-size coll-size
                                        :strlen strlen
                                        :pam pam})]
        (pam-tbl {:depth depth
                  :pam pam
                  :opts opts
                  :map->table-fn map->table-fn
                  :bg-color bg-color
                  :indent-color indent-color})))
  ([pam]
      (format-pam pam {:hsla-seq (-> colors/bracket-colors :rainbow-classic reverse)
                       :depth 0})))

(defn class-string->kws [s]
  (when (string? s)
    (map keyword (string/split s "."))))


;;; piccup regex ............................................................
(def tags "(section|span|line|table|tr|td|ol|div)")
(def tag-and-class-re (re-pattern (str "^" tags ".?(.+)?")))
(def tag-re (re-pattern (str "^" tags "(?:\\..+)?$")))

(defn piccup-tag? [v]
  (and
   (keyword? v)
   (not (nil? (re-find tag-re (name v))))))

(defn piccup-vec? [coll]
  (and (vector? coll)
       (piccup-tag? (first coll))))

(defn tag-w-class [{:keys [coll]}]
  (first coll))

(defn inline-style [{:keys [coll]}]
  (when (-> coll second map?)
    (:style (second coll))))

(defn children [{:keys [coll]}]
  (if (-> coll second map?) (subvec coll 2) (subvec coll 1)))

(defn merged-style-map [{:keys [style-map kws inline-style]}]
  (u/merged-style-map style-map kws inline-style))

(defn tag-parts [{:keys [tag-w-class]}]
  (when (keyword? tag-w-class)
    (re-find tag-and-class-re (name tag-w-class))))

(defn tag [{:keys [tag-parts]}]
  (when-let [tag (second tag-parts)]
    (keyword tag)))

(defn kws [{:keys [tag-parts]}]
  (when-let [style-kws-string (last tag-parts)]
    (class-string->kws style-kws-string)))

(defn new-jsonml-vec [{:keys [merged-style-map tag]}]
  (if merged-style-map
    (let [jsonml-style (u/map->css merged-style-map)]
      [tag {:style jsonml-style}])
    [tag]))

(defn jsonml-vec [m]
  (let [new-vec (new-jsonml-vec m)]
    (vec (concat new-vec (:children m)))))

; TODO put in a slot for optional style map
(defn piccup-vec->jsonml-vec
  "Transforms single piccup vector to an edn representation of jsonml.
   Example:
   (piccup->jsonml
    [:span.error.pam-val {:style {:background-color: \"yellow\"}} 12])
   => [:span {:style {...}} 12]"
  [coll]
  (if (piccup-vec? coll)
    (u/kv-> {:style-map style/style-map :coll coll} $
            [tag-w-class
             inline-style
             children
             tag-parts
             tag
             kws
             merged-style-map]
             (do
               (jsonml-vec $)))
    coll))

(defn piccup->jsonml [coll]
  (w/postwalk piccup-vec->jsonml-vec coll))

(defn edn-depth-max [v]
  (if-let [sub-trees (seq (filter coll? v))]
    (if-not (map? v)
      (inc
       (apply max (map edn-depth-max sub-trees)))
      (do
        (apply max (map edn-depth-max sub-trees))))
    0))

(defn tf [x]
  (when (and (coll? x)
             (> 4 (edn-depth-max x)))
    (clj->js (piccup->jsonml (format-pam x)))))

(def formatter
  "With no folded body, output as head"
  (clj->js {"header" tf
            "hasBody" (fn [x] nil)
            "body" (fn [x] nil)}))

(defonce dtf
  (->> js/window.devtoolsFormatters
       js->clj
       (concat [formatter])
       vec
       clj->js))

(aset js/window "devtoolsFormatters" dtf)
