(ns mgm7734.util
  ;(:use [clojure.tools.trace :only [deftrace]])
  )

(defmacro condl
  "Cond plus :let bindings"
  ([] nil)
  ([a b & body]
    (if (odd? (count body)) (throw (IllegalArgumentException. "condl requires an even number of forms")))
    (if (= a :let)
      `(let ~b (condl ~@body))
      `(if ~a ~b (condl ~@body)))))

(defn count-bound
  "A predicate on the length of x that is safe for infinite x.
  A lazy version of (cmp n (length x))"
  [n cmp x]
  (if (< n 0)
    (if (seq x)
      (cmp n 1)
      (cmp n 0))
;?    (cmp n (count (take (inc n) x)))
    (loop [n n, xs (seq x)]
      (cond (nil? xs) (cmp n 0)
            (== 0 n)  (cmp 0 1)
            :else (recur (dec n) (next xs))))
    ))

(defn- match-vec
  [pat expr]
  (condl (empty? pat) `(= ~pat ~expr)
         :let [[pos-pats rest-pats] (split-with #(not= '& %) pat)]                              
         (not (#{0 2} (count rest-pats))) 
         (throw (IllegalArgumentException. (str "& must be followed by exact one form, not " 
                                                rest-pats)))
         :let [e (gensym "e")
               pos-clauses  (map-indexed (fn [i p] `(match? ~p (get ~e ~i))) pos-pats)
               rest-clauses (if rest-pats
                              [`(match? ~(second rest-pats) (nthnext ~e ~(count pos-pats)))])]
         true
         `(let [~e ~expr] 
            (and (sequential? ~e) (count-bound ~(count pos-pats) <= ~e)
                 ~@pos-clauses ~@rest-clauses)) ))

(defmacro match?
  "Pattern matching test without bindings.  This can be a lot more efficient than match-map or match-let
 pat => {map-pat*} , [pos-pat* rest-pat?] , 'quote-pat, (pred-pat) , #\"re-pat\" , anything-pat , sym-pat or const-pat
 map-pat => key pat ,        matches if expr is a map and pat matches (get expr key)
 pos-pat => pat ,            matches if expr is sequential with values matching every corresponding pat
 rest-pat => & pat ,         matches the rest of a sequential expr
 pred-pat => pred args*      matches if (pred arg* expr) is true
 re-pat  =>                  matches strings
 anything-pat => _           equivalent to [but more efficient than] the pattern ((constantly true)) 
 quote-pat  => anything      literal match. 
 sym-pat => symbol ,         is an error! Variables only allowed in match-map and match-let
 const-pat => anything else, matches when = expr. Note that the pattern is not evaluated, but the expr is.
"
  [pat expr]
  (cond
    (map?        pat)  (let [e (gensym "e")] 
                         `(let [~e ~expr] 
                            (and (map? ~e) ~@(for [[k p] pat] `(match? ~p (get ~e ~k))) )))
    (vector?     pat)   (match-vec pat expr)    
    (and (sequential? pat) (not= 'quote (first pat))) 
                       (concat pat [expr])
    (= '_       pat)   true
    (instance? java.util.regex.Pattern pat)
                       `(re-matches ~pat ~expr)
    (symbol? pat)       (throw (IllegalArgumentException. (str pat ": naked symbols not allowed (yet)")))
    :else              `(= ~pat ~expr) ))

(defmacro merge-match-maps [& mes]
  (let [step (fn step [mes rs]
               (if (empty? mes) 
                 (cons 'merge rs)
                 (let [r (gensym "r")] 
                   `(if-let [~r ~(first mes)] ~(step (rest mes) (conj rs r))))))]
    (step mes [])) )

(defn- match-map-vec
  [pat expr]
  (condl (empty? pat) `(if (= ~pat ~expr) {})
         :let [[pos-pats rest-pats] (split-with #(not= '& %) pat)]                              
         (not (#{0 2} (count rest-pats))) 
         (throw (IllegalArgumentException. (str "& must be followed by exact one form, not " 
                                                rest-pats)))
         :let [e (gensym "e")
               pos-clauses  (map-indexed (fn [i p] `(match-map ~p (get ~e ~i))) pos-pats)
               rest-clauses (if rest-pats
                              [`(match-map ~(second rest-pats) (nthnext ~e ~(count pos-pats)))])]
         true
         `(let [~e ~expr] 
            (and (sequential? ~e) (count-bound ~(count pos-pats) <= ~e)
                 (merge-match-maps ~@pos-clauses ~@rest-clauses))) ))

(defn- expand-directive
  [directive syms sym-to-key]
  (if-not (and (vector? syms) (every? symbol? syms)) 
    (throw (IllegalArgumentException. (str directive " must be followed by a vector of symbols"))))
  (for [s syms] [(sym-to-key s) s]))

(defn- match-map-map
  [pat expr]
  (let [e (gensym "e")
        pat* (mapcat (fn [[k p]]
                       (case k
                         keys> (expand-directive 'keys> p #(keyword (name %)))
                         syms> (expand-directive 'syms> p #(list 'quote %))
                         strs> (expand-directive 'strs> p str)
                         [[k p]]))
                     pat)] 
    `(let [~e ~expr] 
       (and (map? ~e) (merge-match-maps
                        ~@(for [[k p] pat*] `(match-map ~p (get ~e ~k)))) ))))

;(defn make-match-map-key [sym] `'~sym)
(defn make-match-map-key [sym] (keyword sym))

(defmacro match-map
  "Pattern matching with vars"
  [pat expr]
  (cond
    (= '_        pat)   {}
    (symbol? pat)       {(make-match-map-key pat) expr}
    (map?        pat)   (match-map-map pat expr)
    (vector?     pat)   (match-map-vec pat expr)    
    (and (sequential? pat) (not= 'quote (first pat))) 
                       `(if ~(concat pat [expr]) {})
    (instance? java.util.regex.Pattern pat)
                       `(if (re-matches ~pat ~expr) {})
    :else              `(if (= ~pat ~expr) {}) ))
