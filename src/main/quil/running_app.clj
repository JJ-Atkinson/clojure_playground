(ns quil.running-app)


;; this is a no deps file. reason to exist is so tools.namespace never reloads it and causes us to lose access
;; to the reference of the quill application

(defonce sketch-atom (atom nil))

(defonce sketch-draw (atom nil))

(defonce sketch-setup (atom nil))

(defonce sketch-step (atom nil))

(defonce sketch-keydown (atom nil))

(defn quit-app! []
  (when @sketch-atom
    (.exit @sketch-atom)))

(defn mount-app! [app]
  (quit-app!)
  (reset! sketch-atom app))