(ns pretty-print-plus.style
  (:require [pretty-print-plus.util :as util]
            [clojure.string :as string]))

(defn v->str [v]
  (cond
    (or (symbol? v) (string? v) (keyword? v))
    (name v)
    (number? v)
    (str v "px")
    :else (when (vector? v)
            (string/join " " (map v->str v)))))

(defn linear-gradient
  [direction & stops]
  (let [direction (when direction
                    (str
                     (if (number? direction)
                       (str (.parseFloat direction) "deg")
                       (when (or (string? direction)
                                 (keyword? direction))
                         (string/replace (name direction) #"-" " ")))
                     ", "))]
    (str
     "linear-gradient("
     direction
     (string/join ", " (map v->str stops))
     ")")))


(def colors {:mint-bg "hsla(150, 100%, 50%, 0.03)"
             :gray   "hsl(0, 0%, 30%)"
             :blood  "hsl(4, 77%, 63%)"
             :dark-blood "hsl(350, 50%, 63%)"
             :gold   "hsl(52, 99%, 43%)"
             :grass  "hsl(144, 100%, 40%)"
             :skye   "hsl(209, 85%, 60%)"
             :purps  "hsl(273, 80%, 65%)"
             :faint-blood-light   "hsl(10, 24.4%, 96%)"
             :faint-purps-light   "hsl(270, 23.2%, 94.6%)"
             :faint-skye-light    "hsl(206, 22.7%, 93.2%)"
             :faint-skye-medium   "hsl(206, 25.7%, 89.2%)"
             :faint-grass-light   "hsl(142, 24.9%, 93.7%)"
             :faint-grass-medium  "hsl(142, 23.9%, 90.7%)"
             :faint-gold-light    "hsl(51, 36.3%, 92.2%)"

             :f-red     "hsl(330, 56.3%, 98.0%)"
             :f-orange  "hsl(30, 56.3%, 94.5%)"
             :f-yellow  "hsl(45, 56.3%, 88.2%)"
             :f-green   "hsl(110, 56.3%, 90.2%)"
             :f-cyan    "hsl(170, 56.3%, 88.2%)"
             :f-blue    "hsl(230, 56.3%, 92.2%)"
             :f-magenta "hsl(290, 56.3%, 92.2%)"

             :red       "hsl(330, 60%, 50%)"
             :orange    "hsl(30, 60%, 50%)"
             :yellow    "hsl(45, 70%, 40%)"
             :green     "hsl(110, 60%, 50%)"
             :cyan      "hsl(170, 60%, 50%)"
             :blue      "hsl(230, 60%, 50%)"
             :magenta   "hsl(290, 60%, 50%)"
             })

(def rk-colors [:red :orange :yellow :green :cyan :blue :magenta])

(def rk-bg-colors [:f-red :f-orange :f-yellow :f-green :f-cyan :f-blue :f-magenta])

(defn rotate-seq [comment seq start]
  (if (or (= 0 start) (= 1 (count seq)))
    seq
    (concat (subvec (vec seq) start) (subvec (vec seq) 0 start))))

(defn nth-color
  ([depth-idx color-seq start]
    (let [c-idx       (mod depth-idx (count color-seq))
          shifted-seq (rotate-seq "nth-color shifted-seq" color-seq start)
          rkclr-kw    (nth shifted-seq c-idx)]
      (rkclr-kw colors)))
  ([depth-idx color-seq]
    (nth-color depth-idx color-seq 0)))

(defn nth-hsla
  ([depth-idx
    hue-seq
      {:keys [steps start sat val alpha val-seq sat-seq shift-val? shift-sat?]
     :or {steps 7 start 0 sat 100 val 50 alpha 1}}]
    (let [val-seq         (or val-seq (list val))
          sat-seq         (or sat-seq (list sat))
          shifted-hue-seq (rotate-seq "hue" hue-seq start)
          shifted-sat-seq (rotate-seq "sat" sat-seq (if shift-sat? start 0))
          shifted-val-seq (rotate-seq "val" val-seq (if shift-val? start 0))
          hue-idx         (mod depth-idx (count shifted-hue-seq))
          sat-idx         (mod depth-idx (count shifted-sat-seq))
          val-idx         (mod depth-idx (count shifted-val-seq))
          hsla-hue        (nth shifted-hue-seq hue-idx)
          hsla-sat        (nth shifted-sat-seq sat-idx)
          hsla-val        (nth shifted-val-seq val-idx)
          hsla-sat-pct    (str hsla-sat "%")
          hsla-val-pct    (str hsla-val "%")
          hsla            (str "hsla(" hsla-hue ", " hsla-sat-pct ", " hsla-val-pct ", 1)")]
      hsla))
  ([depth-idx hue-seq]
     (nth-hsla depth-idx hue-seq {})))

#_(defn zip-ranges [& ranges]
  (apply map (cons vector ranges)))

(defn zip-ranges [m]
  nil
  #_(zipmap (repeat (keys m)) (vals m)))


(defn circ-range [start end nsteps]
  (let [step-amt (/ (dec (- end start)) nsteps)
        vs       (range start end step-amt)]
    (concat vs (-> (drop 1 vs) drop-last reverse))))

(defn nth-color-kw
  ([depth-idx color-seq start]
    (let [c-idx       (mod depth-idx (count color-seq))
          shifted-seq (rotate-seq "x" color-seq start)
          rkclr-kw    (nth shifted-seq c-idx)]
      rkclr-kw))
  ([depth-idx color-seq]
    (nth-color-kw depth-idx color-seq 0)))

(def fx {:white-text-glow "1px 1px 5px hsla(0, 0%, 100%, .5), -1px -1px 20px hsla(0, 0%, 100%, .5);"})

;;TODO get padding working for spans
(def style-map
  {:Number {:color (-> colors :gray)
            }
   :Function {:color "green" :background-color (-> colors :mint-bg)}
   :Keyword {:color (-> colors :gray) }
   :String {:color (-> colors :gray) }
   :Nil {:font-style "italic"}
   :Boolean {:font-style "italic"}
   :coll-brace {:color "gray"}
   :pam-ob {:color "gray"}
   :pam-cb {:margin-left "0px" :color "gray"}
   :Keyword.pam-key {:color "hsla(315, 100%, 30%, 1)"  :margin-left "0px" :margin-right "0px"}
   :highlight {:background-color "yellow!important"}
   :pink {:background "pink"}
   :red {:color "red" :background "silver"}
   :blue {:color "blue"}
   :neon {:color "lime!important" :background "gray"}
   :default   {:color "gray" :line-height "1.0rem"}
   :error     {:border-bottom "1px solid red"}
   :error-button {:background "red"
                  :color "white"
                  :padding "0 5px"
                  :border-radius "5px"}

  ;; custom formatting
   :pam-table {:border-collapse "collapse" :padding 0 :line-height "1.1rem"}
   :pam-td  {:vertical-align "top" :padding 0 :min-width "0px"}
   :pam-key {:white-space "nowrap"}
   :pam-key-str {:margin-left "-1px"}
   :pam-val {:border-left "7px solid transparent"}
   :seq-val {:border-left "1px solid transparent"}
   :pam-obr {:margin-left "" :font-weight "bold"}
   :pam-cbr {:vertical-align "bottom" :font-weight "bold"}
   :empty-obr {:margin-right "2px"}
   :empty-cbr {:margin-left "2px"}})

(def rk-nsteps 7)
(def hue-seq (range 0 360 (/ 360 rk-nsteps)))
(def val-seq (circ-range 64 54 7))

(def nsteps 7)

(def vls (util/circ-range (util/step-range 80 92 nsteps)))
(def hws (util/step-range-ex 0 360 nsteps))
(def sts (take nsteps (repeat 50)))
