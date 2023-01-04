
(ns clojure.core.typed
  (:refer-clojure :exclude [defrecord type])
  (:import (clojure.lang IPersistentList IPersistentVector Symbol Cons Seqable IPersistentCollection
                         ISeq ASeq ILookup Var Namespace PersistentVector APersistentVector
                         IFn IPersistentStack Associative IPersistentSet IPersistentMap IMapEntry
                         Keyword Atom PersistentList IMeta PersistentArrayMap Compiler Named
                         IRef AReference ARef IDeref IReference APersistentSet PersistentHashSet Sorted
                         LazySeq APersistentMap))
  (:require [analyze.core :refer [ast] :as analyze]
            [analyze.hygienic :as hygienic]
            [clojure.set :as set]
            [clojure.reflect :as reflect]
            [clojure.string :as str]
            [clojure.repl :refer [pst]]
            [clojure.pprint :refer [pprint]]
            [trammel.core :as contracts]
            [clojure.math.combinatorics :as comb]
            [clojure.java.io :as io]
            [cljs
             [compiler]
             [analyzer :as cljs]]
            [clojure.tools.trace :refer [trace-vars untrace-vars
                                         trace-ns untrace-ns]]))

(set! *warn-on-reflection* true)


; constraint shorthands, other handy functions
(load "typed/utils")

;Note: defrecord is now trammel's defconstrainedrecord

;(ann analyze.hygienic/emit-hy [Any -> Any])

;AnalysisExpr -> Form
;(ann emit-form-fn [Any -> Any])
(def emit-form-fn hygienic/emit-hy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special functions

;(ann print-filterset [String Any -> Any])
(defn print-filterset
  "Print the filter set attached to form, and debug-string"
  [debug-string frm] 
  frm)

(declare Method->Function unparse-type unparse-filter)

;(ann method-type [Symbol -> nil])
(defn method-type 
  "Given a method symbol, print the core.typed types assigned to it"
  [mname]
  (let [ms (->> (reflect/type-reflect (Class/forName (namespace mname)))
             :members
             (filter #(and (instance? clojure.reflect.Method %)
                           (= (str (:name %)) (name mname))))
             set)
        _ (assert (seq ms) (str "Method " mname " not found"))]
    (prn "Method name:" mname)
    (doseq [m ms]
      (prn (unparse-type (Method->Function m))))))

;(ann inst-poly [Any Any -> Any])
(defn inst-poly 
  [inst-of types-syn]
  inst-of)

;(ann inst-poly-ctor [Any Any -> Any])
(defn inst-poly-ctor [inst-of types-syn]
  inst-of)

(defmacro inst 
  "Instantiate a polymorphic type with a number of types"
  [inst-of & types]
  `(inst-poly ~inst-of '~types))

(defmacro inst-ctor
  "Instantiate a call to a constructor with a number of types.
  First argument must be an immediate call to a constructor."
  [inst-of & types]
  `(inst-poly-ctor ~inst-of '~types))

;(ann fn>-ann [Any Any -> Any])
(defn fn>-ann [fn-of param-types-syn]
  fn-of)

;(ann pfn>-ann [Any Any -> Any])
(defn pfn>-ann [fn-of polys param-types-syn]
  fn-of)

;(ann loop>-ann [Any Any -> Any])
(defn loop>-ann [loop-of bnding-types]
  loop-of)

;(ann doseq>-ann [Any Any -> Any])
(defn doseq>-ann [the-doseq bnding-types body]
  the-doseq)

;(ann parse-fn> [Any (Seqable Any) ->
;                '{:poly Any
;                  :fn Any ;Form
;                  :parsed-methods (Seqable '{:dom-syntax (Seqable Any)
;                                             :dom-lhs (Seqable Any)
;                                             :rng-syntax Any
;                                             :has-rng? Any
;                                             :body Any})}])
;for
(defn- parse-fn>
  "(fn> name? :- type? [[param :- type]* & [param :- type *]?] exprs*)
  (fn> name? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [is-poly forms]
  (let [name (when (symbol? (first forms))
               (first forms))
        forms (if name (rest forms) forms)
        poly (when is-poly
               (first forms))
        forms (if poly (rest forms) forms)
        methods (if ((some-fn vector? keyword?) (first forms))
                  (list forms)
                  forms)
        ;(fn> name? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
        ; (HMap {:dom (Seqable TypeSyntax)
        ;        :rng (U nil TypeSyntax)
        ;        :body Any})
        parsed-methods (doall 
                         (for [method methods]
                           (let [[ret has-ret?] (when (not (vector? (first method)))
                                                  (assert (= :- (first method))
                                                          "Return type for fn> must be prefixed by :-")
                                                  [(second method) true])
                                 method (if ret 
                                          (nnext method)
                                          method)
                                 body (rest method)
                                 arg-anns (first method)
                                 [required-params _ [rest-param]] (split-with #(not= '& %) arg-anns)]
                             (assert (sequential? required-params)
                                     "Must provide a sequence of typed parameters to fn>")
                             (assert (not rest-param) "fn> doesn't support rest parameters yet")
                             {:dom-syntax (doall (map (comp second next) required-params))
                              :dom-lhs (doall (map first required-params))
                              :rng-syntax ret
                              :has-rng? has-ret?
                              :body body})))]
    {:poly poly
     :fn `(fn ~@(concat
                  (when name
                    [name])
                  (for [{:keys [body dom-lhs]} parsed-methods]
                    (apply list (vec dom-lhs) body))))
     :parsed-methods parsed-methods}))

(defmacro pfn> 
  "Define a polymorphic typed anonymous function.
  (pfn> name? [binder+] :- type? [[param :- type]* & [param :- type *]?] exprs*)
  (pfn> name? [binder+] (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [& forms]
  (let [{:keys [poly fn parsed-methods]} (parse-fn> true forms)]
    `(pfn>-ann ~fn '~poly '~parsed-methods)))

(defmacro fn> 
  "Define a typed anonymous function.
  (fn> name? :- type? [[param :- type]* & [param :- type *]?] exprs*)
  (fn> name? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [& forms]
  (let [{:keys [fn parsed-methods]} (parse-fn> false forms)]
    `(fn>-ann ~fn '~parsed-methods)))

(defmacro defprotocol> [& body]
  "Define a typed protocol"
  `(tc-ignore
     (defprotocol ~@body)))

(defmacro loop>
  "Define a typed loop"
  [bndings* & forms]
  (let [bnds (partition 2 bndings*)
        ; [[lhs :- bnd-ann] rhs]
        lhs (map ffirst bnds)
        rhs (map second bnds)
        bnd-anns (map #(-> % first next second) bnds)]
    `(loop>-ann (loop ~(vec (mapcat vector lhs rhs))
                  ~@forms)
                '~bnd-anns)))

(defmacro declare-datatypes 
  "Declare datatypes, similar to declare but on the type level."
  [& syms]
  `(tc-ignore
  (doseq [sym# '~syms]
    (assert (not (or (some #(= \. %) (str sym#))
                     (namespace sym#)))
            (str "Cannot declare qualified datatype: " sym#))
    (let [qsym# (symbol (str (munge (name (ns-name *ns*))) \. (name sym#)))]
      (declare-datatype* qsym#)))))

(defmacro declare-protocols 
  "Declare protocols, similar to declare but on the type level."
  [& syms]
  `(tc-ignore
  (doseq [sym# '~syms]
     (let [qsym# (if (namespace sym#)
                   sym#
                   (symbol (str (name (ns-name *ns*))) (name sym#)))]
       (declare-protocol* qsym#)))))

(defmacro declare-alias-kind
  "Declare a kind for an alias, similar to declare but on the kind level."
  [sym ty]
  `(tc-ignore
   (do (ensure-clojure)
     (let [sym# '~sym
           qsym# (if (namespace sym#)
                   sym#
                   (symbol (name (ns-name *ns*)) (name sym#)))
           ty# (parse-type '~ty)]
       (assert (not (namespace sym#)) (str "Cannot declare qualified name " sym#))
       (declare ~sym)
       (declare-names ~sym)
       (declare-alias-kind* qsym# ty#)))))

(defmacro declare-names 
  "Declare names, similar to declare but on the type level."
  [& syms]
  `(tc-ignore
  (doseq [sym# '~syms]
     (let [qsym# (if (namespace sym#)
                   sym#
                   (symbol (name (ns-name *ns*)) (name sym#)))]
       (declare-name* qsym#)))))

(defmacro def-alias 
  "Define a type alias"
  [sym type]
  `(tc-ignore
  (do (ensure-clojure)
    (let [sym# (if (namespace '~sym)
                 '~sym
                 (symbol (name (ns-name *ns*)) (name '~sym)))
          ty# (parse-type '~type)]
      (add-type-name sym# ty#)
      (declare ~sym)
      (when-let [tfn# (@DECLARED-KIND-ENV sym#)]
        (assert (subtype? ty# tfn#) (error-msg "Declared kind " (unparse-type tfn#)
                                               " does not match actual kind " (unparse-type ty#))))
      [sym# (unparse-type ty#)]))))

(declare Type? RClass? PrimitiveArray? RClass->Class parse-type symbol->Class
         requires-resolving? -resolve Nil? Value? Value->Class Union? Intersection?)

;Return a Class that generalises what this Clojure type will look like from Java,
;suitable  for use as a Java primitive array member type.
; 
; (Type->array-member-Class (parse-type 'nil)) => Object
; (Type->array-member-Class (parse-type '(U nil Number))) => Number
; (Type->array-member-Class (parse-type '(Array (U nil Number)))) =~> (Array Number)

;(ann Type->array-member-Class (Fn [Type -> (Option Class)]
;                                  [Type Any -> (Option Class)]))
(defn Type->array-member-Class 
  ([ty] (Type->array-member-Class ty false))
  ([ty nilok?]
   {:pre [(Type? ty)]}
   (cond
     (requires-resolving? ty) (Type->array-member-Class (-resolve ty) nilok?)
     (Nil? ty) (if nilok?
                 nil
                 Object)
     (Value? ty) (Value->Class ty)
     ;; handles most common case of (U nil Type)
     (Union? ty) (let [clss (map #(Type->array-member-Class % true) (:types ty))
                       prim-and-nil? (and (some nil? clss)
                                          (some #(when % (.isPrimitive ^Class %)) clss))
                       nonil-clss (remove nil? clss)]
                   (if (and (= 1 (count nonil-clss))
                            (not prim-and-nil?))
                     (first nonil-clss)
                     Object))
     (Intersection? ty) Object
     (RClass? ty) (RClass->Class ty)
     (PrimitiveArray? ty) (class (make-array (Type->array-member-Class (:jtype ty) false) 0))
     :else Object)))

;(ann into-array>* [Any Any -> Any])
(defn into-array>* 
  ([cljt coll]
   (into-array (-> cljt parse-type Type->array-member-Class) coll))
  ([javat cljt coll]
   (into-array (-> javat parse-type Type->array-member-Class) coll)))

(defmacro into-array> 
  "Make a Java array with Java class javat and Typed Clojure type
  cljt. Resulting array will be of type javat, but elements of coll must be under
  cljt. cljt should be a subtype of javat (the same or more specific)."
  ([cljt coll]
   `(into-array>* '~cljt ~coll))
  ([javat cljt coll]
   `(into-array>* '~javat '~cljt ~coll)))

(defn ann-form* [form ty]
  form)

(defmacro ann-form [form ty]
  `(ann-form* ~form '~ty))

;(ann unsafe-ann-form* [Any Any -> Any])
(defn unsafe-ann-form* [form ty]
  form)

(defmacro unsafe-ann-form [form ty]
  `(unsafe-ann-form* ~form '~ty))

;(ann tc-ignore-forms* [Any -> Any])
(defn tc-ignore-forms* [r]
  r)

;; `do` is special at the top level
(defmacro tc-ignore 
  "Ignore forms in body during type checking"
  [& body]
  `(do ~@(map (fn [b] `(tc-ignore-forms* ~b)) body)))

(defmacro non-nil-return 
  "Override the return type of method msym to be non-nil.
  Takes a set of relevant arities,
  represented by the number of parameters it takes (rest parameter counts as one),
  or :all which overrides all arities.
  
  eg.  (non-nil-return java.lang.Class/getDeclaredMethod :all)"
  [msym arities]
  `(tc-ignore
  (add-nonnilable-method-return '~msym '~arities)))

(defmacro nilable-param 
  "Overrides which parameters in a method may accept
  nilable values. If the parameter is a parameterised type or
  an Array, this also declares the parameterised types and the Array type as nilable.

  mmap is a map mapping arity parameter number to a set of parameter
  positions (integers). If the map contains the key :all then this overrides
  other entries. The key can also be :all, which declares all parameters nilable."
  [msym mmap]
  `(tc-ignore
  (add-method-nilable-param '~msym '~mmap)))

(declare abstract-many instantiate-many)

(load "typed/type_rep"
      "typed/type_ops"
      "typed/filter_rep"
      "typed/filter_ops"
      "typed/path_rep"
      "typed/object_rep")

; must be after type/object/filter definitions
(load "typed/fold")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotations

(declare TCResult?)

;(ann (predicate (APersistentMap Symbol Any)))
(def lex-env? (hash-c? (every-pred symbol? (complement namespace)) Type?))

(defrecord PropEnv [l props]
  "A lexical environment l, props is a list of known propositions"
  [(lex-env? l)
   (every? Filter? props)])

(declare ^:dynamic *lexical-env*)

(defn print-env [debug-str]
  nil)

(defn print-env*
  ([] (print-env* *lexical-env*))
  ([e]
   {:pre [(PropEnv? e)]}
   ;; DO NOT REMOVE
   (prn {:env (into {} (for [[k v] (:l e)]
                         [k (unparse-type v)]))
         :props (map unparse-filter (:props e))})))

(defonce VAR-ANNOTATIONS (atom {}))
(def ^:dynamic *lexical-env* (->PropEnv {} []))

(defmacro with-lexical-env [env & body]
  `(binding [*lexical-env* ~env]
     ~@body))

(set-validator! VAR-ANNOTATIONS #(and (every? (every-pred symbol? namespace) (keys %))
                                      (every? Type? (vals %))))
(set-validator! #'*lexical-env* PropEnv?)

(defmacro ann [varsym typesyn]
  `(tc-ignore
 (do (ensure-clojure)
   (let [t# (parse-type '~typesyn)
         s# (if (namespace '~varsym)
              '~varsym
              (symbol (-> *ns* ns-name str) (str '~varsym)))]
     (do (add-var-type s# t#)
       [s# (unparse-type t#)])))))

(declare parse-type alter-class*)
