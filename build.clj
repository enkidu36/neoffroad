(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'my/starter)
(def version "1.0.0")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file  "neoffroad.jar")

(defn clean [_]
  (b/delete {:path "target"})
  (b/delete {:path uber-file}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main 'neoffroad.core}))

