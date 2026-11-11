(ns app.system
  (:require [integrant.core :as ig]
            [app.core :as core]))

(defmethod ig/init-key :app.system/store
  [_ opts]
  (core/map->MapStore opts))

(defmethod ig/init-key :app.system/handler
  [_ {:keys [store]}]
  (fn [req] (core/greet (:name req))))

(def config
  {:app.system/store {:state {}}
   :app.system/handler {:store (ig/ref :app.system/store)}})
