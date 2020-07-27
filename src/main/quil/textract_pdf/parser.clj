(ns quil.textract-pdf.parser
  (:require [quil.core :as q]
            [quil.running-app :as ra]
            [meander.epsilon :as m]
            [clojure.set :as set]
            [zprint.core :as zp]))


(defonce pdf-read (->> "document-parsed-aws-textract.edn" slurp read-string))
(defonce by-id (into {} (map (fn [x] [(:Id x) x]) pdf-read)))

(defonce highlighted-ids (atom #{}))
(def page (atom 2))
(def size-multiplier (atom 1900))
(defonce selected-id (atom nil))
(def conj-set (fnil conj #{}))

(defn highlight-id! [id] (swap! highlighted-ids set/union (set (cond-> id (string? id) vector))))
(defn unhighlight-id! [id] (swap! highlighted-ids disj id))
(defn clear-highlights! [] (reset! highlighted-ids #{}))
(comment
  @highlighted-ids
  (highlight-id! "id"))

(comment (let [ids (m/search
                     pdf-read
                     (m/scan {:Id   ?id
                              :Text (m/re #"Farm")})
                     ?id)]
           (highlight-id! ids))
  (clear-highlights!))


(do
  (defn setup [& args]
    (q/frame-rate 4)
    (q/text-font (q/create-font "Ubuntu" 16)))
  (reset! ra/sketch-setup setup))

(def words-on-page
  (memoize (fn [page-num]
             (m/search pdf-read
               (m/scan {:BlockType "WORD"
                        :Page      ~page-num
                        :Geometry  {:BoundingBox ?pos}
                        :as        ?q})
               (merge ?q ?pos)))))

(defn distance [x1 y1 x2 y2]
  (Math/sqrt (+ (Math/pow (- x1 x2) 2)
               (Math/pow (- y1 y2) 2))))
(comment (distance 1 1 2 2))

(defn nearest-mouse [blocks]            ;; must have the position mounted top level
  (let [mx (/ (q/mouse-x) @size-multiplier)
        my (/ (q/mouse-y) @size-multiplier)]
    (reduce (fn [[dist cblock] nblock]
              (let [ndist (distance (:Left nblock) (:Top nblock) mx my)]
                (if (< ndist dist)
                  [ndist nblock]
                  [dist cblock])))
      [100 nil]
      blocks)))


(defn draw-word [{:keys [Id Text Width Height Left Top custom-fill]}]
  (q/fill 255)
  (q/stroke 255)
  (when (@highlighted-ids Id) 
    (q/fill 255 128 128))
  (when custom-fill
    (q/fill custom-fill))
  (when Text
    (q/text Text (* @size-multiplier Left) (* @size-multiplier Top))))


(do (defn draw [& args]
      (q/background 0)
      (let [words (words-on-page @page)
            [dist block] (nearest-mouse words)]
        (doall (map draw-word words))
        (q/text (str "Dist: " dist) (- (q/width) 120) 20)
        (q/text (str "FPS: " (q/current-frame-rate)) (- (q/width) 120) 40)
        (q/text (zp/zprint-str block 70) (- (q/width) 750) 20)
        (when (q/mouse-pressed?)
          (reset! selected-id (:Id block)))
        (draw-word (assoc block :custom-fill (q/color 128 255 128)))))
    (reset! ra/sketch-draw draw))





(defn mount-sketch []
  (q/defsketch application
    :title "PDF Debugger"
    :setup @ra/sketch-setup
    :draw (fn [& args] (apply @ra/sketch-draw args))
    :size [1600 1500]
    :features [:resizable])
  (ra/mount-app! application))


(comment (mount-sketch)

  (q/available-fonts))