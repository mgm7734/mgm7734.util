(ns mgm7734.util
  (:use [clojure.tools.trace :only [deftrace tracer trace]])
  )

(defmacro condl
  "Cond plus :let bindings"
  ([] nil)
  ([a b & body]
    (if (odd? (count body)) (throw (IllegalArgumentException. "condl requires an even number of forms")))
    (if (= a :let)
      `(let ~b (condl ~@body))
      `(if ~a ~b (condl ~@body)))))

(defn reducer
  ""
  ([f coll]
    (if-let [[x & xs] (seq coll)]
      (if xs
        (f x (reducer f xs))
        x)
      (f) ))
  ([f val coll]
    (if-let [[x & xs] (seq coll)]
      (f x (reducer f val xs))
      val)))


(defn count-bound
  "A predicate on the length of x that is safe for infinite x.
  A lazy version of (cmp n (length x))"
  [n cmp x]
  (if (< n 0)
    (if (seq x)
      (cmp n 1)
      (cmp n 0))
    (loop [n n, xs (seq x)]
      (cond (nil? xs) (cmp n 0)
            (== 0 n)  (cmp 0 1)
            :else (recur (dec n) (next xs))))
    ))

;;;================ match?

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
 pat => {map-pat*} , [pos-pat* rest-pat?] , 'quote-pat, (pred-pat) , #\"re-pat\" , anything-pat , var-pat or const-pat
 map-pat => key pat ,        matches if expr is a map and pat matches (get expr key)
 pos-pat => pat ,            matches if expr is sequential with values matching every corresponding pat
 rest-pat => & pat ,         matches the rest of a sequential expr
 pred-pat => pred args*      matches if (pred arg* expr) is true
 re-pat  =>                  matches strings
 anything-pat => _      ,    equivalent to [but more efficient than] the pattern ((constantly true)) 
 quote-pat  => anything ,    literal match. 
 var-pat => symbol ,         is an error! Variables only allowed in match-map and match-let
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

;;; =============== match-map

(defmacro merge-match-maps [& mes]
  (let [step (fn step [mes rs]
               (if (empty? mes) 
                 (cons 'merge rs)
                 (let [r (gensym "r")] 
                   `(if-let [~r ~(first mes)] ~(step (next mes) (conj rs r))))))]
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
                         keys (expand-directive 'keys p #(keyword (name %)))
                         syms (expand-directive 'syms p #(list 'quote %))
                         strs (expand-directive 'strs p str)
                         [[k p]]))
                     pat)] 
    `(let [~e ~expr] 
       (and (map? ~e) (merge-match-maps
                        ~@(for [[k p] pat*] `(match-map ~p (get ~e ~k)))) ))))

;(defn make-match-map-key [sym] `'~sym)
(defn make-match-map-key [sym] (keyword sym))

(defmacro match-map
  "Pattern matching with vars yielding a map of var values"
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

;;;==================== match-case

(defmacro with-fresh-var 
  "Given (with-fresh-var x e `(foo ~x)) is (let [x (gensym)] `(let [~x ~e] (foo ~x))) except

Like `(let [~x ~e] fx) when ~e is a constant or variable symbol, otherwise like (let [g# e  
Returns an expression that: if e is a constant or a variable sysmbol,  evaluates fx with x = e,
otherwise evalutes fx with x = a fresh symbol bound to e.  Prevents e from being evaluated multiple times in fx."
  [x e fx]
  `(if ((some-fn number? string? char? keyword? symbol?) ~e)
     (let [~x ~e] ~fx)
     (let [~x (gensym)]
       (list 'let [~x ~e] ~fx))))

(declare parse-pat)

(defn- parse-map
  [map-pat expr]
  (condl
    (empty? map-pat)  {:type :bool, :value (with-fresh-var e expr `(and (map? ~e) (empty? ~e)))}
    
    :let [pat* (mapcat (fn [[k p]]
                         (case k
                           keys> (expand-directive 'keys> p #(keyword (name %)))
                           syms> (expand-directive 'syms> p #(list 'quote %))
                           strs> (expand-directive 'strs> p str)
                           [[k p]]))
                       map-pat)
          fresh-sym (gensym "e")
          children (for [[k p]  pat*] (parse-pat p `(get ~fresh-sym ~k)))
          vars (mapcat :vars children) ]
    true
    {:type :map, :vars vars, :expr expr, :free-var fresh-sym , :children children}
    ))

(defn- parse-vector
  [vec-pat expr]
  (condl
    (empty? vec-pat) {:type :bool, :value (list '= vec-pat expr)}
    
    :let [[pos-pats rest-pats] (split-with #(not= '& %) vec-pat)]                              
    (not (#{0 2} (count rest-pats))) 
    (throw (IllegalArgumentException. (str "& must be followed by exact one form, not " 
                                           rest-pats)))
    :let [v           (gensym "e")
          pos-children (map-indexed (fn [i p] (parse-pat p `(get ~v ~i))) pos-pats)
          rest-child   (if (seq rest-pats) (parse-pat (second rest-pats) `(nthnext ~v ~(count pos-pats))))
          vars (concat (mapcat :vars pos-children) (:vars rest-child))]
    true
    {:type :vector, :vars vars, :expr expr, :free-var v, :pos-children pos-children, :rest-child rest-child}  ))

(defn- parse-pat 
  [pat expr]
  (condp (fn [test _] (test pat)) pat
    #{'_}         {:type :bool,  :value true}
    symbol?       {:type :bind,  :vars [pat] , :value expr}
    map?         (parse-map pat expr)
    vector?      (parse-vector pat expr)  
    sequential?  (case (first pat)
                   quote {:type :bool, :value `(= ~pat ~expr)}
                   :| (let [expr* (gensym "e")
                            pat* (filter (partial not= '|) pat)
                            children (map #(parse-pat % expr*) (next pat))]
                        (if (some #(not= [(:type %) (:vars %)] [:bool []]) children) 
                          (throw (IllegalArgumentException. "Variables not allowed in :| exprs")))
                        {:type :bool, :value `(let [~expr* ~expr] 
                                                ;; more efficient than (or ...) for constants
                                                ~(reducer (fn [a b] `(if ~a ~a ~b)) 
                                                          (map :value children)))})
                   {:type :bool , :value (concat pat [expr])})  
    ;; TODO: (re-pat sym...) to bind groups to vars
    (partial instance? java.util.regex.Pattern)   {:type :bool ,  :value `(re-matches ~pat ~expr)}
    {:type :bool, :value `(= ~pat ~expr)} ))

(defn- eval-node [n body]
  (let [reduce-step (fn [pos-node inner-result]
                      (eval-node pos-node inner-result) )]
    (case (:type n)
      :bool  `(if ~(:value n) ~body)
      :bind  `(let [~(first (:vars n)) ~(:value n)] ~body)
      :vector (let [v (:free-var n)]
                `(let [~v ~(:expr n)]
                   (if (sequential? ~v) 
                     ~(if (:rest-child n)
                        `(if (count-bound ~(count (:pos-children n)) <= ~v)
                           ~(reducer reduce-step
                               (eval-node (:rest-child n) body)
                               (:pos-children n)))
                        `(if (count-bound ~(count (:pos-children n)) == ~v)
                           ~(reducer reduce-step
                               body
                               (:pos-children n) ))) )))
      :map (let [v (:free-var n)]
             `(let [~v ~(:expr n)]
                (if (map? ~v)
                  ~(reducer reduce-step body (:children n)))))
      )    )
)
(defmacro match-case
  "Pattern matching with local bindindgs, guards and results"
  [expr pat & body]
  `(first ~(eval-node (parse-pat pat expr) [`(do ~@body)]) )
  
                                        ;  (cond
                                        ;    (= '_        pat)   {}
                                        ;    (symbol? pat)       {(make-match-map-key pat) expr}
                                        ;    (map?        pat)   (match-map-map pat expr)
                                        ;    (vector?     pat)   (match-map-vec pat expr)    
                                        ;    (and (sequential? pat) (not= 'quote (first pat))) 
                                        ;                       `(if ~(concat pat [expr]) {})
                                        ;    (instance? java.util.regex.Pattern pat)
                                        ;                       `(if (re-matches ~pat ~expr) {})
                                        ;    :else              `(if (= ~pat ~expr) {}) )
  
  )


