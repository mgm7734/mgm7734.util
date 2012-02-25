(ns mgm7734.util
;  (:require [clojure.tools.trace :as tr])
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
  (condl :let [[pos-pats rest-pats] (split-with #(not= '& %) pat)]
                              
         (not (#{0 2} (count rest-pats))) 
         (throw (IllegalArgumentException. (str "& must be followed by exact one form, not " 
                                                rest-pats)))
         :let [pos-clauses  (map-indexed (fn [i p] `(match? ~p (get ~expr ~i))) pos-pats)
               rest-clauses (if rest-pats
                              [`(match? ~(second rest-pats) (nthnext ~expr ~(count pos-pats)))])]
         true
         `(and (sequential? ~expr) (count-bound ~(count pos-pats) <= ~expr)
               ~@pos-clauses ~@rest-clauses) ))

(defmacro match?
  "pat => {map-pat*} , [pos-pat* rest-pat?] , (pred-pat) , #\"re-pat\" , anything-pat , 'lit-sym-pat, or const-pat
 map-pat => key pat ,        matches if expr is a map and pat matches (get expr key)
 pos-pat => pat ,            matches if expr is sequential with values matching every corresponding pat
 rest-pat => & pat ,         matches the rest of a sequential expr
 pred-pat => pred args*      matches if (pred arg* expr) is true
 re-pat  =>                  matches strings
 anything-pat => :_          equivalent to [but more efficient than] the pattern ((constantly true)) 
 lit-sym-pat  => symbol      literal match.  unquoted reserved
 const-pat => anything else, matches when = expr. Note that the pattern is not evaluated, but the expr is.
"
  [pat expr]
  (cond
    (map?        pat)  `(and (map? ~expr) ~@(for [[k p] pat] `(match? ~p (get ~expr ~k))) )
    (vector?     pat)   (match-vec pat expr)    
    (and (sequential? pat) (not= 'quote (first pat))) 
                       (concat pat [expr])
    (= ':_       pat)   true
    (instance? java.util.regex.Pattern pat)
                       `(re-matches ~pat ~expr)
    (symbol? pat)       (throw (IllegalArgumentException. (str pat ": naked symbols not allowed (yet)")))
    :else              `(= ~pat ~expr) ))
