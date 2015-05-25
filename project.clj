(defproject vention "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.7.0-beta1"]
                 [org.clojure/clojurescript "0.0-3269"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.omcljs/om "0.8.8"]
                 [sablono "0.3.4"]]

  :node-dependencies [[source-map-support "0.2.8"]]

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

  :plugins [[lein-cljsbuild "1.0.6"]
            [com.cemerick/austin "0.1.6"]
            [lein-kibit "0.0.8"]]

  :source-paths ["src/cljs"]

  :clean-targets ["out/cljs/rob" "out/cljs/rob.js"]

  :profiles
  {:dev
   {:cljsbuild
   { :builds
    [ {:source-paths ["src/cljs"]
       :compiler
       { :output-to "out/cljs/rob.js"
        :output-dir "out/cljs"
        :optimizations :none
        :cache-analysis true
        :source-map true}}]}}
   :cdn
   {:omit-source true
    :aot :all
    :cljsbuild
    { :builds
     [ {:source-paths ["src/cljs"]
        :compiler
        { :output-to "out/rob.js"
         :optimizations :advanced
         :static-fns true
         :externs ["src/externs/d3.externs.js"]
         :pretty-print false
         }} ]}}})
