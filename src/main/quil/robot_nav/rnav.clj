(ns quil.robot-nav.rnav
  (:require [quil.core :as q]
            [quil.core :as quil]
            [quil.running-app :as ra]
            [meander.epsilon :as m]
            [clojure.set :as set]
            [clojure.pprint :as pprint]
            [zprint.core :as zp]
            [medley.core :as mc]
            [clojure.string :as str]
            [org.nfrac.cljbox2d.testbed.tests.raycast :as raycast]


            [org.nfrac.cljbox2d.core :refer :all :as b2]
            [quil.middleware]
            [quil.running-app :as ra]
            [org.nfrac.cljbox2d.vec2d :refer [polar-xy v-add TWOPI PI] :as v2]
            [org.nfrac.cljbox2d.testbed :as bed]
            [taoensso.timbre :as log]))


(def HALFPI (/ PI 2))






(def length 11.0)
(def eye [0 10])

(defn f "Takes ft [in]? and converts it to world cords."
  ([f i]
   ;; 5 is a nice scale factor
   (* 5 (+ f (* i 1/12))))
  ([f] (* 5 f)))

(def board-width (f 0 1.5))


(defn vec->world-box
  "Special fn to convert a vector specifiying cords into world cords. If an '_ is encountered, it is replaced
   with `board-width`"
  [world v]
  (let [[x y w h :as pos] (mapv #(if (= '_ %) board-width %) v)]
    (assert (every? some? pos))

    (body! world {:type           :static
                  :position       [(+ x (/ w 2)) (+ y (/ h 2))]
                  :fixed-rotation true
                  :user-data      {}}
      {:shape (box (/ w 2) (/ h 2))})))

(def initial-world
  (let [width (f 8)
        height (f 4)
        bw board-width]
    (vec (mapcat
           (fn [vv?] (if (vector? (first vv?)) vv? (vector vv?)))
           [; world boarders
            [[0 0 width '_]
             [0 (- height bw) (f 8) '_]
             [0 0 '_ height]
             [(- width bw) 0 '_ height]]

            ; A
            [[(f 1 6) bw bw (f 0 8)]
             [bw (f 0 8) (- (f 1 6) bw) '_]]

            ; A | B
            [(+ (f 1 6) bw (f 0 10)) bw bw (f 0 8)]

            ; B
            (let [b-start (+ (f 3 5))]
              [[b-start bw bw (f 0 8)]
               [(+ b-start bw (f 0 11)) bw bw (f 0 8)]
               [(+ b-start bw) (f 0 8) (f 0 11) bw]])

            ; B | C
            [(- width (f 1 6) bw (f 0 10) bw) bw bw (f 0 8)]

            ; C
            [[(- width (f 1 6) bw) bw bw (f 0 8)]
             [(- width (f 1 6)) (f 0 8) (f 1 6) bw]]

            ; D
            (let [dy (+ bw (f 0 8) (f 0 10))
                  d-inner-w (f 2)]
              [[bw dy d-inner-w '_]
               [bw (+ dy (f 0 5.5) (- bw)) d-inner-w '_]
               [d-inner-w dy '_ (f 0 5.5)]])

            ; Centerline T
            (let [w (f 2 1)
                  _y (+ bw (f 0 8) (f 0 10))
                  _x (+ (/ width 2) (- (/ w 2)))]
              [[_x _y w '_]
               [(- (/ width 2) (/ bw 2)) (+ _y bw) '_ (f 0 (+ 3 7/8))]])

            ; E
            (let [ey (+ bw (f 0 8) (f 0 10))
                  e-inner-w (f 2)
                  left-bounds (- width bw e-inner-w bw)]
              [[(+ bw left-bounds) ey '_ (f 0 5.5)]
               [(+ left-bounds bw) ey e-inner-w '_]
               [(+ left-bounds bw) (+ ey (f 0 5.5) (- bw)) e-inner-w '_]])

            ; F
            (let [_y (- height bw (f 0 10) bw)
                  _x-hook (- (+ (f 2) bw) (f 1))]
              [[_x-hook _y (f 1) '_]
               [(f 2) (+ bw _y) '_ (f 0 10)]])

            ; _ _
            (let [_x (+ (f 2) bw (f 0 10))
                  w (/ (- width
                         (* 2 (+ (f 2) bw))
                         (* 3 (f 0 10)))
                      2)
                  _y (- height bw (f 0 10) bw)]
              [[_x _y w '_]
               [(+ _x (f 0 10) w) _y w '_]])

            ; H
            (let [_x (- width (f 2) bw)
                  _y (- height bw (f 0 10) bw)]
              [[_x _y (f 1) '_]
               [_x (+ _y bw) '_ (f 0 10)]])
            ]))))



(def initial-raycast
  {:mode      :closest
   :angle     0.0
   :end-point eye})

(def shapes
  [(polygon [[-0.5 0] [0.5 0] [0 1.5]])
   (polygon [[-0.1 0] [0.1 0] [0 1.5]])
   (polygon (for [i (range 8)
                  :let [angle (* TWOPI (/ i 8))]]
              (polar-xy 0.5 angle)))
   (box 0.5 0.5)
   (circle 0.5)])



(defn record-raycast
  [state]
  (let [prev (::raycast state)
        prev-angle (:angle prev)
        angle (+ prev-angle (* 0.25 (/ PI 180)))
        end-point (v-add eye (polar-xy length angle))
        hits (raycast (:world state) eye end-point (:mode prev)
               :ignore (fn [fixt]
                         (= 0 (:shape-idx (user-data (body-of fixt))))))
        rc (assoc prev
             :angle angle
             :end-point end-point
             :hits hits)]
    (assoc state ::raycast rc)))


(defn angle-between [a v-normal]
  (let [b (v2/v-angle v-normal)
        ;; move -PI PI to 0 TWOPI
        a (+ PI a)
        b (+ PI b)]
    (- (if (> a b)
         (as-> (- a b) ab (if (> ab HALFPI) (- ab HALFPI) ab))
         (as-> (- b a) ba (if (> ba HALFPI) (- ba HALFPI) ba)))
      (/ PI 2))))



(defn measure-dist [state start-point angle]
  (let [ray-length (f 8)
        end-point (v-add start-point (polar-xy ray-length angle))
        [initial-hit] (raycast (:world state) start-point end-point :closest
                        :ignore (fn [fixt] (:bot? (user-data (body-of fixt)))))
        hit-point (or (:point initial-hit) end-point)
        ]
    (assoc state ::bot-raycasts [{:start-point start-point
                                  :end-point   hit-point
                                  :length      (v2/v-dist start-point hit-point)
                                  :normal      (when-let [n (:normal initial-hit)]
                                                 (angle-between angle n))
                                  :bounce?     (when-let [n (:normal initial-hit)]
                                                 (> (Math/abs (angle-between angle n)) (/ PI 4)))
                                  }])))



(defn push-bot! [state amount]
  (let [b (::bot state)
        c (center b)]
    (apply-force! b amount c)
    state))

(defn step-actions [state]
  (let [actions {\k #(push-bot! % [0 40])
                 \j #(push-bot! % [0 -40])
                 \h #(push-bot! % [-40 0])
                 \l #(push-bot! % [40 0])}]
    (reduce (fn [state [key down?]]
              (if (and down? (get actions key))
                ((get actions key) state)
                state))
      state
      (::keys-down state))))



(do
  (defn step
    [state]
    (let [bot (::bot state)]
      (-> (bed/world-step state)
        (measure-dist (center bot) (angle bot))
        step-actions
        (record-raycast)
        (bed/record-snapshot true [::raycast ::bot-raycasts]))))
  (reset! ra/sketch-step step))

(defn draw-additional
  [scene ->px]
  (let [rc (or (::raycast scene)
             initial-raycast)
        mode (:mode rc)
        hits (:hits rc)]
    (quil/fill 255)
    (quil/text (str "hjkl to move\n"
                 "fps/target: " (q/current-frame-rate) "/" (q/target-frame-rate) "\n"
                 "Mode = " mode)
      10 10)
    
    (when-let [[hit] (::bot-raycasts scene)]
      (q/line (->px (:start-point hit)) (->px (:end-point hit)))
      (apply q/text
        (str (Math/round (:length hit)) "/" (Math/round (q/degrees (:normal hit 0)))
          (when (:bounce? hit) "------------")) 
        (->px (:end-point hit)) ))
    
    
    (when (or (empty? hits)
            (= mode :all))
      (quil/line (->px eye)
        (->px (:end-point rc))))
    
    (doseq [hit hits]
      (quil/line (->px eye) (->px (:point hit)))
      (let [[x-px y-px] (->px (:point hit))]
        (quil/ellipse x-px y-px 5 5)))))






(do
  (defn my-key-press
    [state event]
    (let [char (:raw-key event)]
      (case char

        \m (let [mode (:mode (::raycast state))
                 new-mode (case mode
                            :closest :all
                            :all :closest)]
             (assoc-in state [::raycast :mode] new-mode))
        ;; otherwise pass on to testbed
        (bed/key-press state event))))
  (reset! ra/sketch-keydown my-key-press))


(defn create-bot [world]
  (let [b (body! world {:position       [(/ (f 8) 2) (- (f 4) board-width (f 0 2) (f 0 3.5))]
                        :user-data      {:bot? true}
                        :gravity-scale  0

                        :linear-damping 0.2}
            {:shape (box (f 0 3.5) (f 0 3.5))})]
    b))



(do
  (defn setup [& args]
    ;(q/frame-rate 4)
    (q/text-font (q/create-font "Ubuntu" 12))
    (quil/frame-rate 60)
    (let [world (new-world)]
      (assoc bed/initial-state
        :world world
        ::raycast initial-raycast
        ::bodies (mapv #(vec->world-box world %) initial-world)
        ::bot (create-bot world))))
  (reset! ra/sketch-setup setup))

(defn key-tracker
  "Given down-or-up?, state, and event, will set ::keys-down {raw-key down-or-up?} in state"
  [down-or-up? state event]
  (assoc-in state [::keys-down (:raw-key event)] down-or-up?))

(do (defn draw
      [state]
      (bed/draw state)
      (let [{:keys [snapshots steps-back camera]} state
            scene (nth snapshots steps-back nil)
            ->px (bed/world-to-px-fn camera)]
        (draw-additional scene ->px)))
    (reset! ra/sketch-draw draw))


(defn mount-sketch []


  (q/defsketch application
    :mouse-pressed bed/mouse-pressed
    :mouse-released bed/mouse-released
    :mouse-dragged bed/mouse-dragged
    :mouse-wheel bed/mouse-wheel
    :size [600 500]
    :features [:resizable]
    :middleware [quil.middleware/fun-mode]

    :key-pressed (partial key-tracker true)
    :key-released (partial key-tracker false)

    :title "RNav"
    :setup @ra/sketch-setup
    :draw (fn [& args] (apply @ra/sketch-draw args))
    :update (fn [s] (if (:paused? s) s (@ra/sketch-step s)))
    :key-typed (fn [& args]
                 (apply @ra/sketch-keydown args)
                 #_(case (q/key-as-keyword)
                     ;:j (next-p!)
                     ;:k (prev-p!)
                     nil)))
  (ra/mount-app! application))


(comment
  (ra/mount-app! (raycast/-main))
  (require 'org.nfrac.cljbox2d.testbed.tests.slider-crank)
  (ra/mount-app! (org.nfrac.cljbox2d.testbed.tests.slider-crank/-main))
  (require 'org.nfrac.cljbox2d.testbed.tests.varying-restitution)
  (ra/mount-app! (org.nfrac.cljbox2d.testbed.tests.varying-restitution/-main))
  (require 'org.nfrac.cljbox2d.testbed.tests.edge-points)
  (ra/mount-app! (org.nfrac.cljbox2d.testbed.tests.edge-points/-main))
  )


(comment
  (mount-sketch)
  (ra/quit-app!)

  (q/available-fonts))
