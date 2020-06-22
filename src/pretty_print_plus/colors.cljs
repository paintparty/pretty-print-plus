(ns pretty-print-plus.colors
  (:require [pretty-print-plus.util :as u]))

;;; HSL RANGES .........................................................

;cljs devtools theme
(def cljs-devtools-rgb
  {:meta-text                   [238 238 238]
   :protocol                    [41 59 163 ]
   :ns                          [150 150 150]
   :fn                          [30 130 30]
   :fn-args                     [170 130 20]
   :nil                         [128 128 128]
   :keyword                     [136 19 145]
   :integer                     [28 0 207]
   :float                       [28 136 207]
   :float-nan                   [213 60 27]
   :float-infinity              [28 80 207]
   :string                      [196 26 22]
   :expanded-string             [255 100 100]
   :symbol                      [0 0 0]
   :bool                        [0 153 153]})


(def cljs-devtools-hsl
  {:fn              {:h 120, :s 62, :l 31}
   :protocol        {:h 231, :s 60, :l 40}
   :expanded-string {:h 0, :s 100, :l 69}
   :float           {:h 204, :s 76, :l 46}
   :ns              {:h 0, :s 0, :l 59}
   :symbol          {:h 0, :s 0, :l 0}
   ;;  :string          {:h 1.4, :s 80, :l 43}
   ;; Let's do green instead of red^ for the strings
   :string          {:h 103, :s 56, :l 35}
   :integer         {:h 248, :s 100, :l 41}
   ;;  :keyword         {:h 296, :s 50, :l 23}
   ;; Let's do slate blue instead of purps^ for the keywords
   :keyword         {:h 240, :s 50, :l 33}
   :nil             {:h 0, :s 0, :l 50}
   :fn-args         {:h 44, :s 79, :l 37}
   :bool            {:h 180, :s 100, :l 30}
   :meta-text       {:h 0, :s 0, :l 93}
   :float-infinity  {:h 223, :s 76, :l 46}
   :float-nan       {:h 11, :s 77, :l 47}
   :atom            {:h 200, :s 61, :l 61}})


;; SETS and lists are BROKEN
;Rainbow classic
(def rainbow-classic
  {:red       {:h 335 :s 60 :l 50}
   :orange    {:h 27  :s 60 :l 50}
   :yellow    {:h 60  :s 65 :l 40}
   :green     {:h 110 :s 50 :l 45}
   :cyan      {:h 180 :s 65 :l 38}
   :blue      {:h 230 :s 60 :l 60}
   :magenta   {:h 290 :s 60 :l 50}})

;Rainbow classic lightened
(def rainbow-classic-light
  {:red       {:h 335 :s 60 :l 80}
   :orange    {:h 27  :s 60 :l 80}
   :yellow    {:h 60  :s 65 :l 70}
   :green     {:h 110 :s 50 :l 75}
   :cyan      {:h 180 :s 65 :l 68}
   :blue      {:h 230 :s 60 :l 90}
   :magenta   {:h 290 :s 60 :l 80}})

;Rainbow magic
#_(def rk-clrs
  {:red       {:h 335 :s 60 :l 50}
   :magenta   {:h 290 :s 60 :l 50}
   :blue      {:h 230 :s 60 :l 60}
   :cyan      {:h 180 :s 65 :l 38}
   :green     {:h 110 :s 50 :l 45}
   :yellow    {:h 60  :s 65 :l 40}
   :orange    {:h 27  :s 60 :l 50}
   })

;Opposing staggered
#_(def rk-clrs
  {:red       {:h 335 :s 60 :l 50}
   :green     {:h 110 :s 50 :l 45}
   :magenta   {:h 290 :s 60 :l 50}
   :cyan      {:h 180 :s 65 :l 38}
   :orange    {:h 27  :s 60 :l 50}
   :blue      {:h 230 :s 60 :l 60}
   :yellow    {:h 60  :s 65 :l 40}
   })

;Opposing staggered, lightened for indents
#_(def rk-clrs-light
  {:red       {:h 335 :s 60 :l 80}
   :green     {:h 110 :s 50 :l 75}
   :magenta   {:h 290 :s 60 :l 80}
   :cyan      {:h 180 :s 65 :l 68}
   :orange    {:h 27  :s 60 :l 80}
   :blue      {:h 230 :s 60 :l 90}
   :yellow    {:h 60  :s 65 :l 70}
   })

#_(def rk-bg-clrs
  {:red       {:h 335 :s 30 :l 98}
   :orange    {:h 27  :s 30 :l 96.57}
   :yellow    {:h 60  :s 30 :l 95.14}
   :green     {:h 110 :s 20 :l 93.71}
   :cyan      {:h 180 :s 30 :l 92.29}
   :blue      {:h 230 :s 30 :l 90.86}
   :magenta   {:h 290 :s 25 :l 89.43}})

(def rk-bg-clrs-gray
  {
   :100 {:h 0 :s 0 :l 100}
   :98 {:h 0 :s 0 :l 97}
   :96 {:h 0 :s 0 :l 94}
   :982 {:h 0 :s 0 :l 97}
   })


(def bg-color-seq-tuned
  '(
   {:h 335, :s 90, :l 98.1} ; rm
   {:h 27, :s 75, :l 95.2} ; orange
   {:h 50, :s 45, :l 91} ; yellow
   {:h 110, :s 27, :l 90.2} ; green
   {:h 170, :s 27, :l 88.2} ; aqua
   {:h 230, :s 25, :l 89.55} ; blue
   {:h 290, :s 26, :l 87.15} ; purps
   {:h 330, :s 30, :l 85.8} ; midpoint rm
   {:h 27, :s 36, :l 86.03} ; orange
   {:h 50, :s 38, :l 85.3} ; yellow
   {:h 110, :s 27, :l 89.2} ; green
   {:h 180, :s 33, :l 90.4} ; aqua
   {:h 230, :s 60, :l 95.4} ; blue
   {:h 290, :s 65, :l 96.5} ; purps
    ))

(def bg-l
  '(98.5 95 90 88 85 88 86 84 84.5 88 87 88 90 95))

(def nsteps 7)

(def hws (u/step-range-ex 0 360 nsteps))

(def sts (take nsteps (repeat 50)))

(def bg-clr-m {:h (map :h (vals rk-bg-clrs-gray))
               :s (map :s (vals rk-bg-clrs-gray))
               :l bg-l})

(def bg-clr-seq-gray (vals rk-bg-clrs-gray))

(def indent-colors
  {:rainbow-classic (vals rainbow-classic-light)})

(def bg-colors {:neutral (vals rk-bg-clrs-gray)
                :rainbow-classic bg-color-seq-tuned})

(def bracket-colors
  {:rainbow-classic (vals rainbow-classic)})

;;; Color us ...........................................................
(defn hsla-string [h s l a]
    (str "hsla(" (js/Math.round h) ", " s "%, " l "%, " a ")"))

(defn nth-hsla
  ([hsla-seq depth]
   (nth-hsla hsla-seq depth 1))
  ([hsla-seq depth alpha]
   (let [val-idx (mod depth (count hsla-seq))
         m       (nth hsla-seq val-idx)
         h       (or (:h m) 0)
         s       (or (:s m) 100)
         l       (or (:l m) 50)
         a       (or (:a m) alpha)]
     (hsla-string h s l a))))
