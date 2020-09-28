(ns quil.textract-pdf.parser
  (:require [quil.core :as q]
            [quil.running-app :as ra]
            [meander.epsilon :as m]
            [clojure.set :as set]
            [clojure.pprint :as pprint]
            [zprint.core :as zp]
            [medley.core :as mc]
            [clojure.string :as str]))


(defn integrate-bounding-box [x]
  (merge x (-> x :Geometry :BoundingBox)))


(defonce highlighted-ids (atom #{}))
(def page (atom 2))
(defn next-p! [] (swap! page inc))
(defn prev-p! [] (swap! page dec))
(comment (next-p!) (prev-p!))
(def size-multiplier (atom 1050))
(defonce selected-id (atom nil))
(def conj-set (fnil conj #{}))
(def std-line-height 0.015)

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
    (q/text-font (q/create-font "Ubuntu" 12)))
  (reset! ra/sketch-setup setup))


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







(defn integrate-bounding-box [x]
  (merge x (-> x :Geometry :BoundingBox)))

(def ^:dynamic *all-words* (->>
                             ;"/home/jarrett/Downloads/blocks.edn"
                             "/home/jarrett/code-projects/jra-aws/NewSite/cool-blocks.edn"
                             ;"/home/jarrett/Downloads/cool-blocks.edn"
                             slurp read-string (map integrate-bounding-box)))
(def ^:dynamic *element-by-id* (into {} (map (fn [x] [(:Id x) x]) *all-words*)))

(def + (fnil clojure.core/+ 0 0 0))
(def - (fnil clojure.core/- 0 0 0))
(def > (fnil clojure.core/> 0 0))
(def < (fnil clojure.core/< 0 0))


(def words-on-page
  (memoize (fn [page-num]
             (m/search *all-words*
               (m/scan {:BlockType "WORD"
                        :Page      ~page-num
                        :as        ?q})
               ?q))))


(defn same?
  ([a b] (same? a b 0.003))
  ([a b tolerance]
   (if (and a b)
     (< (Math/abs (- a b)) tolerance)
     false)))

(defn same-row?
  ([e1 e2] (same? (:Top e1) (:Top e2)))
  ([e1 e2 tolerance] (same? (:Top e1) (:Top e2) tolerance)))

(defn close?
  ([origin] (close? origin 0.003))
  ([origin tolerance]
   (fn [n] (same? n origin tolerance))))

(defn below?
  ([origin] (below? origin 0.003))
  ([origin tolerance]
   (fn [n] (if (and origin tolerance n)
             (< (- origin tolerance) n)
             false))))

(defn above?
  ([origin] (above? origin 0.003))
  ([origin tolerance]
   (fn [n] (> (+ origin tolerance) n))))

(def right? below?)
(def ahead? below?)
(def left? above?)
(def behind? above?)

(defn on-line

  ([page line-y]
   (on-line page line-y 0.003))
  ([page line-y tolerance]
   (m/search (words-on-page page)
     (m/scan {:Id  ?id
              :Top (m/pred (close? line-y tolerance))})
     ?id)))

(defn on-n-lines [page start-y num-lines]
  (m/search *all-words*
    (m/scan {:Id   ?id
             :Page ~page
             :Top  (m/and
                     (m/pred (below? start-y))
                     (m/pred (above? (+ start-y (* std-line-height num-lines)))))})
    ?id))

(comment :highlight-all-on-selected-line
  (clear-highlights!)
  (->
    (on-line @page
      (->> @selected-id
        *element-by-id*
        :Top)
      0.004)
    highlight-id!)
  (->
    (on-n-lines @page
      (->> @selected-id
        *element-by-id*
        :Top)
      2)
    highlight-id!))

(defn photo-line-headers []
  (m/search *all-words*
    (m/scan {:Id   ?id
             :Left (m/pred (behind? 0.03))
             :Text (m/re #"Phot.*")})
    ?id))


(defn photo-number-legal-lines []
  (let [header-ids (photo-line-headers)
        headers (map #(get *element-by-id* %) header-ids)]
    (mapcat #(on-n-lines (:Page %) (:Top %) 2) headers)))

(comment :highlight-photo-groups
  (clear-highlights!)
  (->
    (photo-line-headers)
    highlight-id!)
  (->
    (photo-number-legal-lines)
    highlight-id!))

(defn remove-garbage [cb]
  (let [garbage-ids (photo-number-legal-lines)
        garbage-ids-set (set garbage-ids)]
    (binding [*all-words* (remove #(contains? garbage-ids-set (:Id %)) *all-words*)
              *element-by-id* (apply dissoc *element-by-id* garbage-ids)]
      (cb))))

(defn farm-headings
  "Grabs all instances of \"Farm\" on the left of the page"
  [words]
  (m/search words
    (m/scan {:Id   ?id
             :Left (m/pred (close? 0 0.05))
             :Text (m/re #"Far.*")})
    ?id))

(comment :highlight-farm-of-headers
  (clear-highlights!)
  (->> pdf-read
    farm-headings
    highlight-id!))


(defn collect-headings [some-element]
  (on-line (:Page some-element) (:Top some-element)))


(comment :highlight-headers
  (clear-highlights!)
  (->> pdf-read
    farm-headings
    (map by-id)
    (mapcat collect-headings)
    highlight-id!))


(defn collect-left-bounds [header-ids]
  (let [initial-tag
        (map (fn [hid]
               (let [tag (condp re-matches (or (:Text (get *element-by-id* hid)) "")
                           #"Far.*" :farm
                           #"Tract" :tract
                           #"CLU.*" :clu-field
                           #"Irr[i\.](?:gation)?.*" :irrigation-practice
                           #"Crop/" :crop-commodity
                           #"Var.*" :var-type
                           #"Int.*" :int-use
                           #"Act.*" :act-use
                           #"Org.*" :org.-status
                           #"Nat[t\.](?:ive)?.*" :native-sod
                           #"C.C.*" :cc-status
                           #"Rpt.*" :rpt
                           #"Reported.*" :reported-quantity
                           #"Determined.*" :determined-quantity
                           #"Crop" :crop-land
                           #"Planting.*" :planting
                           #"End.*" :end-date
                           #"Producer.*" :producer
                           #"FSA.*" :fsa-physical-location
                           #"NAP.*" :nap-unit
                           #"P\/P.*" :planting-period
                           #"Sig.*" :signature-date ;; ignored 
                           #".*" nil)]
                 [tag (assoc (get *element-by-id* hid)
                        :tag tag)]))
          header-ids)

        ;; dedupe producer and planting
        producer (m/search initial-tag
                   (m/scan [:producer ?p])
                   ?p)

        planting (m/search initial-tag
                   (m/scan [:planting ?p])
                   ?p)
        rpt (m/search initial-tag
              (m/scan [:rpt ?p])
              ?p)
        mapped (dissoc (into {} initial-tag) :producer :planting :rpt nil)
        [producer-share producer-name] (sort-by :Left producer)
        [planting-date planting-period] (sort-by :Left planting)
        [rpt-unit reported-quantity] (sort-by :Left rpt)]
    (assoc mapped
      :producer-share (assoc producer-share :tag :producer-share)
      :producer-name (assoc producer-name :tag :producer-name)
      :planting-date (assoc planting-date :tag :planting-date)
      :planting-period (or
                         (:planting-period mapped)
                         (assoc planting-period :tag :planting-period))
      :rpt-unit (assoc rpt-unit :tag :rpt-unit)
      :reported-quantity (or
                           (:reported-quantity mapped)
                           (assoc reported-quantity
                             :tag :reported-quantity
                             :Left (+ 0.008 (:Left rpt-unit)))))))

(comment :grabbing-left-bounds
  (->> *all-words*
    farm-headings
    first *element-by-id*
    collect-heading->s
    collect-left-bounds
    (mc/map-vals :Left)
    seq
    (sort-by second)))




(defn determine-y-locations
  "Given some initial block (just to grab page, left and top) try to find the y positions of all the rows. will
  use the `text-match?` fn to determine weather or not there is a row entry at that location. Will return a list of word
  ids. 
  
  Edge case: if there are many instances of this table on the same page, this may end up grabbing the y location of
  the rows of other tables. to prevent this, the minimum y distance is set by the initial start block and the maximum
  is set by the first non matching block in the y column
  
  Edge case: for e.g. consider this layout:
  
  CLU/    <-- this is passed in
  Field
  12A
  13b
  89
  CLU/
  Field
  99
  23
  54
  
  We of course want the actual rows 12A, 13b, and 89, but not any below. But, CLU should not pass our :text-match?
  (nor should Field). We want the first contiguous block of passing elements. a combination of take-while and
  drop-while are used to achieve this."
  [initial-start-block {:keys [x-slop text-match? line-skip]
                        :or   {x-slop    0.03 text-match? (constantly true)
                               line-skip 2}}]
  (let [all-matching (m/search (words-on-page (:Page initial-start-block))
                       (m/scan {:Left (m/pred (close? (:Left initial-start-block) x-slop))
                                :Top  (m/pred (below? (:Top initial-start-block)))
                                :as   ?match})
                       ?match)
        sorted-descending (sort-by :Top all-matching)
        text-m-fn #(text-match? (:Text %))]

    (->> sorted-descending
      (drop-while (complement text-m-fn))
      (take-while text-m-fn))))

(comment "Click CLU field first! The text-match? fn is tuned for it"
  (clear-highlights!)
  (highlight-id! (map :Id
                   (-> @selected-id
                     *element-by-id*
                     (determine-y-locations {:text-match? #(re-matches #"\d+.*" %)})))))

(defn create-bounded-pairs
  "convert a sorted list of numbers to a bounded set.
   [0.1 0.2 0.3] =>
   [[0   0.1]
    [0.1 0.2]
    [0.2 0.3]
    [0.3 1  ]]"
  [list]
  (map vector (concat [0] list) (concat list [1])))



(defn tag-table [some-header-block]
  (let [header-elements (collect-headings some-header-block)
        tagged-headers (collect-left-bounds header-elements)
        CLU-header (:clu-field tagged-headers)
        ys (sort (map :Top (determine-y-locations CLU-header {:text-match? #(re-matches #"\d+.*" %)})))
        ordered-xs (sort-by :Left (vals tagged-headers))
        max-x 1
        max-y (+ (or (last ys) 0) 0.025)
        all-words-in
        (m/search (words-on-page (:Page some-header-block))
          (m/scan {:Left (m/and (m/pred (ahead? 0)) (m/pred (behind? 1)) ?left)
                   :Top  (m/and (m/pred (below? (first ys))) (m/pred (above? max-y)) ?top)
                   :as   ?inside})
          (assoc ?inside
            :header (:tag (last (take-while (comp (behind? ?left) :Left) ordered-xs)))
            :row (count (take-while (behind? ?top) ys))))
        ]
    all-words-in))

;
(defn tagged-table->entities [tagged-table]
  (let [groups (vals (group-by :row tagged-table))
        rows (sort-by (comp :row first) groups)
        rows (map (fn [rw] (reduce (fn [m e] (update m (:header e) conj e)) {} rw)) rows)]
    rows))

(comment :table-tag
  (clear-highlights!)
  (->> @selected-id *element-by-id* tag-table (map :Id) highlight-id!)
  (->> @selected-id *element-by-id* tag-table (map #(select-keys % [:Text :header :Left])))
  (->> @selected-id *element-by-id* tag-table tagged-table->entities)
  )






; check
(defn compare-position
  "Sort by Y first, then X. "
  ([e1 e2]
   (compare-position e1 e2 0.003))
  ([e1 e2 y-tolerance]
   (if (same-row? e1 e2 y-tolerance)
     (compare (:Left e1) (:Left e2))
     (compare (:Top e1) (:Top e2)))))

; check
(defn mmap-vals [f seq-map]
  (map (fn [m] (mc/map-vals f m)) seq-map))

; check
(defn compact-words [words]
  (if (>= 1 (count words))
    (str (:Text (first words)))
    (let [words (sort compare-position words)]
      (:Text (reduce (fn [prior curr]
                       (update curr :Text #(str (:Text prior)
                                             (if (same-row? prior curr) " " "\n")
                                             %)))
               words)))))



; check
(defn compact-entities
  [row-entities]
  (mmap-vals compact-words row-entities))



(comment
  (compact-words [{:Text "hi"}])
  (compact-words [{:Text "hi" :Top 0 :Left 0}
                  {:Text "hi2" :Top 0 :Left 2}])
  (compact-words [{:Text "hi2" :Top 2 :Left 0}
                  {:Text "hi" :Top 0 :Left 0}])
  (compact-words [{:Text "hi2" :Top 2 :Left 0}
                  {:Text "hi3" :Top 2 :Left 1}
                  {:Text "hi" :Top 0 :Left 0}]))


(def field-order [:farm
                  :tract
                  :clu-field
                  :irrigation-practice
                  :crop-commodity
                  :var-type
                  :int-use
                  :act-use
                  :org.-status
                  :native-sod
                  :cc-status
                  :rpt-unit
                  :reported-quantity
                  :determined-quantity
                  :crop-land
                  :planting-date
                  :end-date
                  :producers
                  :fsa-physical-location
                  :nap-unit])

(comment :resolved-table
  (clear-highlights!)
  (->> @selected-id *element-by-id* tag-table tagged-table->entities compact-entities (pprint/print-table field-order))
  )


(defn custom-row-correction
  "producer name will often bleed over into the producer share. this will correct that
  e.g. 
  ...    Producer Share      Producer Name
          1.00        2ndary   Owner
          99.00       Primray  Owner
  
   This will parse to:
   {:producer-share \"1.00 2ndary\\n99.00 Primary\" :producer-name \"Owner\\nOwner\"}
   
   The final result will be a map called :producers of share -> name"
  [r]
  (let [{ps :producer-share pn :producer-name} r
        ps (str/split-lines (or ps ""))
        pn (str/split-lines (or pn ""))
        producer-share-split (map (fn [ps]
                                    (let [[whole-match share owner-name]
                                          (re-matches #"(\d+\.\d{2})(?:\ ([\w\-]+))?" ps)]
                                      [(Float/parseFloat (or share "0"))
                                       owner-name])) ps)
        producers (into {} (map (fn [[share partial-name] rest-of-name]
                                  [share (str partial-name " " rest-of-name)])
                             producer-share-split
                             pn))]
    (-> r (assoc :producers producers)
      (update :fsa-physical-location (fnil str/replace "") "\n" " "))))


(comment :final-construction
  (->> @selected-id *element-by-id* tag-table tagged-table->entities compact-entities (map custom-row-correction)
    (pprint/print-table field-order)))


(defn parse-table [some-header-id]
  (->> some-header-id
    (get *element-by-id*)
    tag-table
    tagged-table->entities
    compact-entities
    (map custom-row-correction)))

(defn parse-document []
  (map parse-table (farm-headings *all-words*)))







(comment
  (clear-highlights!)
  ;; parse the whole doc
  (mapcat identity (parse-document))

  ;; highlight tables
  (remove-garbage
    #(doall (map (fn [hid] (->> hid *element-by-id* tag-table (map :Id) highlight-id!))
              (farm-headings *all-words*))))

  (time
    (remove-garbage
      (fn [] (spit "final-csv.csv"
               (str (str/join "\t" field-order) "\n"
                 (str/join "\n" (map (fn [row] (str/join "\t" (map (comp #(str/replace % "\n" " ") str row) field-order)))
                                  (mapcat identity (parse-document))))))))))




(defn address-section-ids []
  (m/search (words-on-page 1)
    (m/scan {:Top  (m/and (m/pred (below? 0.07)) (m/pred (above? 0.14)))
             :Left (m/pred (behind? 0.21))
             :Id   ?id})
    ?id))

(comment (highlight-id! (address-section-ids)))


(defn get-address []
  (let [words (compact-words (map *element-by-id* (address-section-ids)))
        [_ farm-name street city-abbr state zip]
        (re-matches #"Producer Name and Address\n(.*)\n(\d+[\w+ ?]+)\n(.*), (\w\w) (\d+-\d+)"
          words)]
    {:farm-name farm-name :street street :city-abbr city-abbr :state state :zip zip}))

(comment (println (get-address))
  (re-matches #"Producer Name and Address\n(.*)\n(\d+[\w+ ?]+)\n(.*), (\w\w) (\d+-\d+)"
    "Producer Name and Address\nKAL-MAC FARMS\n4690 SPRING PLACE CV E\nOLIVE BRANCH, MS 38654-8128"))


(comment :final-construction
  (->> @selected-id by-id tag-table tagged-table->entities compact-entities (map custom-row-correction)
    (pprint/print-table field-order)))








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
      (remove-garbage
        #(do

           (q/text-align :left :top)
           (q/background 0)
           (let [words (words-on-page @page)
                 [dist block] (nearest-mouse words)]
             (doall (map draw-word words))
             (q/text (str "Dist: " dist) (- (q/width) 120) 20)
             (q/text (str "FPS: " (q/current-frame-rate)) (- (q/width) 120) 40)
             (q/text (zp/zprint-str block 70) (- (q/width) 550) 20)
             (when (q/mouse-pressed?)
               (reset! selected-id (:Id block))
               (q/background 255))
             (draw-word (assoc block :custom-fill (q/color 128 255 128)))))))
    (reset! ra/sketch-draw draw))


(defn remove-garbage-once_DEV! []
  (let [garbage-ids (photo-number-legal-lines)
        garbage-ids-set (set garbage-ids)]
    (let [aw (remove #(contains? garbage-ids-set (:Id %)) *all-words*)
          eid (apply dissoc *element-by-id* garbage-ids)]
      (def ^:dynamic *all-words* aw)
      (def ^:dynamic *element-by-id* eid))))

(comment (remove-garbage-once_DEV!))




(defn mount-sketch []
  (q/defsketch application
    :title "PDF Debugger"
    :setup @ra/sketch-setup
    :draw (fn [& args] (apply @ra/sketch-draw args))
    :key-typed (fn [] (case (q/key-as-keyword)
                        :j (next-p!)
                        :k (prev-p!)
                        nil))
    :size [1600 1500]
    :features [:resizable])
  (ra/mount-app! application))


(comment
  (mount-sketch)
  (ra/quit-app!)

  (q/available-fonts))
