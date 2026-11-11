(ns jolt-lsp.watch
  "Polling watcher. No host watch service is assumed on Jolt."
  (:require [babashka.fs :as fs]
            [jolt-lsp.paths :as paths]))

(defn snapshot [root]
  (into {}
        (map (fn [p] [p (str (fs/last-modified-time p))]))
        (paths/source-files root)))

(defn start!
  "Poll every interval-ms; call (on-change) when the file set or mtimes change.
  Returns a stop atom."
  [root on-change interval-ms]
  (let [stop (atom false)]
    (future
      (loop [prev (snapshot root)]
        (Thread/sleep (or interval-ms 1000))
        (when-not @stop
          (let [cur (snapshot root)]
            (when (not= prev cur) (on-change))
            (recur cur)))))
    stop))

(defn stop! [stop]
  (reset! stop true))
