(ns user
  (:require
    [clojure.tools.namespace.repl :as tools-ns :refer [set-refresh-dirs]]
    ))

(set-refresh-dirs "src/main" "src/dev" "src/test")





(defn restart
  "Stop, reload code, and restart the server. If there is a compile error, use:

  ```
  (tools-ns/refresh)
  ```

  to recompile, and then use `start` once things are good."
  []
  (tools-ns/refresh))

(defn restart-dev
  "Stop, reload code, and restart the server. If there is a compile error, use:

  ```
  (tools-ns/refresh)
  ```

  to recompile, and then use `start` once things are good."
  []
  (tools-ns/refresh))
