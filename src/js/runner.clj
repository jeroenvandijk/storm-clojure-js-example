(ns js.runner
  (:import (org.mozilla.javascript Context Function Scriptable ScriptableObject NativeObject  NativeArray Undefined)
          (org.mozilla.javascript Parser CompilerEnvirons ErrorReporter Token  Node))
  )

;; ----------------------------------
;; Serialization Rhino JS <-> Clojure
;; Stolenn from http://mu-labs.googlecode.com/svn/trunk/eng/tools/js-clojure-bridge/src/cljrt.clj

(defn- sanitize
  "Sanitizes Clojure names."
  [s]
  (.replaceAll (str s) "[\\W_]+" "_"))

(defn- lazy-arr 
  "Convers a NativeArray to a lazy Clojure sequence."
  [arr ids]
  (if (empty? ids)
    nil 
    (let [nx (.get arr (first ids) nil)]
      (lazy-seq (cons nx (lazy-arr arr (rest ids)))))))

(defn to-clj 
  "Convert Rhino JavaScript object to Clojure data."
  [obj]
  (letfn [(to-key     [s]   (keyword (sanitize s)))
          (value-for  [id]  (.get obj id nil))
          (keys-for   [ids] (map to-key ids))
          (values-for [ids] (map value-for ids))]
    (cond 
      (instance? NativeObject obj) 
        (let [ids (.getIds obj)] 
          (zipmap (keys-for ids) (values-for ids)))
      (instance? NativeArray obj)
        (let [ids (.getIds obj)]
          (lazy-arr obj ids))
      :else obj)))

(defn to-js 
  "Convert Clojure data to Rhino JavaScript object."
  [data scope]
  (let [ctx (Context/getCurrentContext)]
    (.setJavaPrimitiveWrap (.getWrapFactory ctx) false)
    (letfn [(to-name [s]       (if (instance? clojure.lang.Named s) 
                                 (name s)
                                 (str s)))
            (new-obj []        (.newObject ctx scope))
            (put     [k v obj] (.put obj (to-name k) obj v))
            (to-arr  [arr]     (to-array (map #(to-js % scope) arr)))]
      (cond 
        (map? data) 
          (let [obj (new-obj)] 
            (doseq [k (keys data)] (put k (to-js (data k) obj) obj)) 
            obj)
        (or (seq? data) (vector? data)) 
            (.newArray ctx scope (to-arr data))
        :else (Context/javaToJS data scope)))))


; 
; http://mxr.mozilla.org/mozilla/source/js/rhino/examples/DynamicScopes.java
(defn evaluate-js [script]
  (let [cx (Context/enter)
        ; TODO look into the setup of the scope, this is expensive and can be optimized
        scope (.initStandardObjects cx)
        http (.newObject cx scope)
        log (.newObject cx scope)]

    (try
      ; TODO set each method individually doesn't seem very dry. Can't we do some introspection on our modules?


      ; Set the request object by serializing json, inspired by http://stackoverflow.com/questions/4143184/java-object-to-json
      (.evaluateString cx scope "" "<cmd>" 1 nil)

     ; TODO look into the security domain parameter (nil)
      (let [result (.evaluateString cx scope script "<cmd>" 1 nil)]
        ; (Context/toString result)
        (to-clj result))
     (finally (Context/exit)))))

(comment 
  (def arr (evaluate-js "'1 1'.split()"))
  
  )