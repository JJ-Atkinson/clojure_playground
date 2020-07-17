(ns scratch.overcast-xml
  (:require [meander.epsilon :as m]
            [clojure.xml :as xml]
            [clojure.java.io :as io])
  )


(defn str->bytes [str]
  (java.io.ByteArrayInputStream. (.getBytes str)))

(defn parse [in-str]
  (xml/parse (io/make-input-stream in-str {})))

(defn walk-struct->map [x]
  (clojure.walk/postwalk (fn [x] (if (map? x) (into {} x) x)) x))

;; clojure.xml creates structs, which cannot be dissoced. Causes issues for meander.

(def subscriptions-xml
  (-> "/home/jarrett/Downloads/overcast.opml"
    io/as-file
    parse
    walk-struct->map))


(defn gather-remote-xml [url]
  (-> url io/as-url parse walk-struct->map))

(defonce rss-feed-cache (atom {}))
(defn add-url [{:keys [title rss-url]}] (swap! rss-feed-cache assoc title (gather-remote-xml rss-url)))

(defn to-file [] (spit "scratch.edn" @rss-feed-cache))
(defn from-file [] (reset! rss-feed-cache (read-string (slurp "scratch.edn"))) nil)

(comment 
  (count @rss-feed-cache)
  (-> (get @rss-feed-cache "The Joe Rogan Experience") :content (get 0) :content  ))


(def subscriptions-w-urls
  (m/search subscriptions-xml
    (m/$ {:type       "rss"
          :title      ?title
          :subscribed ?subscribed
          :xmlUrl     (m/some ?rss-feed)})
    {:title ?title :subscribed? (= "1" ?subscribed) :rss-url ?rss-feed}))

(comment (do (doall (pmap add-url subscriptions-w-urls)) nil)
  (to-file)
  (from-file))


(def episodes
  (m/search subscriptions-xml
    (m/$ {:attrs   {:xmlUrl (m/some ?rss-feed)
                    :title  ?feed-title}
          :content (m/scan {:tag   :outline
                            :attrs {:progress (m/some ?progress)
                                    :title    (m/some ?title)}})})
    {:type :in-progress :rss ?rss-feed :progress ?progress :title ?title :feed-title ?feed-title}

    (m/$ {:attrs   {:xmlUrl (m/some ?rss-feed)
                    :title  ?feed-title}
          :content (m/scan {:tag   :outline
                            :attrs {:played "1"
                                    :title  (m/some ?title)}})})
    {:type :played :rss ?rss-feed :title ?title :feed-title ?feed-title}))





(comment (:content subscriptions-xml)


  {:tag :outline :attrs ?attr}

  (def r {:tag :outline :attrs 1})
  (m/search r
    (m/$ {:tag :outline :attrs ?attr})
    ?attr)

  (m/search k
    (m/$ {:n 1 :attr ?a})
    ?a))


(comment
  {:a ["a" "b" "c"]
   :b {:c '(\a \b \c)
       :k {:R :l}}
   :n [{:a :b}
       {:c :d}
       {:e :f}]}

  {:a [string?]
   :b {:c '(char?)
       :k {:R keyword?}}
   :n [{keyword? keyword?}]}


  (m/search [[{:a 2 :b :cool} {:a 2 :b :cooler} {:a 3 :b :coolest} {:a 4 :b :not-cool}]
             [{:a 1 :b :cool} {:a 6 :b :cooler} {:a 7 :b :coolest} {:a 8 :b :not-cool}]]
    (m/scan (m/and
              (m/scan {:a 1 :b ?b-one})
              (m/scan {:a (m/or 4 7) :b (m/some ?b)})
              ))
    [?b-one ?b])


  (m/search (-> @rss-feed-cache (get "The Joe Rogan Experience"))
    (m/$ (m/and 
           (m/scan {:tag :title :content ["#1503 - Josh Barnett"]})
           (m/scan {:tag :enclosure :attrs {:length (m/some ?length)}})))
    (Integer/parseInt ?length)))
