(ns mgm7734.util
  (:use [clojure.tools.trace :only [deftrace tracer trace]])
  )

(defn maybe [x] (if x [x]))
(defn just [x] (list x))
(def none      nil)
(defmacro mget [mx default] `(let [mx# mx] (if mx# mx# default)))

(defmacro condl
  "Cond plus :let bindings"
  ([] nil)
  ([a b & body]
    (if (odd? (count body)) (throw (IllegalArgumentException. "condl requires an even number of forms")))
    (if (= a :let)
      `(let ~b (condl ~@body))
      `(if ~a ~b (condl ~@body)))))

(defn reducer
  "reduce from right to left. "
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
  (cond  
    (counted? x) (cmp n (count x))
    (< n 0) (if (seq x)
              (cmp n 1)
              (cmp n 0))   
    true  (loop [n n, xs (seq x)]
            (cond (nil? xs) (cmp n 0)
                  (== 0 n)  (cmp 0 1)
            :else (recur (dec n) (next xs))))
    ))

;;;==================== match-case

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

(defmacro fast-or
  ([] nil)
  ([x]  x)
  ([x & xs] `(if ~x true (fast-or ~@xs))))

(declare parse-pat)

(defn- parse-map
  [map-pat expr]
  (condl
    (empty? map-pat)  {:type :bool, :value (with-fresh-var v expr `(and (map? ~v) (empty? ~v)))}
    
    :let [pat* (mapcat (fn [[k p]]
                         (case k
                           keys (expand-directive 'keys p #(keyword (name %)))
                           syms (expand-directive 'syms p #(list 'quote %))
                           strs (expand-directive 'strs p str)
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
                   
                   :val (if (= (count pat) 2)
                          {:type :bool, :value `(= ~expr (eval ~(second pat)))}
                          (throw (IllegalArgumentException. ":val requires one argument")))
                   
                   :or (let [v (gensym "e")
                            children (map #(parse-pat % v) (next pat))]
                        (if (some #(seq (:vars %)) children) 
                          (throw (IllegalArgumentException. "Variables not allowed in :or patterns")))
                        {:type :or, :expr expr, :free-var v, :children children } )
                   
                   :as (if (= 3 (count pat))
                         {:type :as,  :var (nth pat 2), :value expr :child (parse-pat (nth pat 1) (nth pat 2))}
                         (throw (IllegalArgumentException. (str "expected (:as pattern var), got: " pat))))
                   
                   (fn fn*) {:type :bool, :value `(~pat ~expr)}
                   ;; (foo..)  => (foo.. expr)
                   {:type :bool, :value (concat pat [expr])})  
    (partial instance? java.util.regex.Pattern)   {:type :bool ,  :value `(re-matches ~pat ~expr)}
    {:type :bool, :value `(= ~pat ~expr)} ))

(defn- eval-node
  "n is compiled pattern, Return expr that evals n and if true, evals body else nil"
  [n body]
  (let [reduce-step (fn [pos-node inner-result]
                      (eval-node pos-node inner-result) )]
    (case (:type n)
      :bool  `(if ~(:value n) ~body)
      :bind  `(let [~(first (:vars n)) ~(:value n)] ~body)
      :or     `(let [~(:free-var n) ~(:expr n)]
                 (if (or ~@(map #(eval-node % []) (:children n))) ~body))
      ;; pat
      :as     `(let [~(:var n) ~(:value n)] ~(eval-node (:child n) body))
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
                  ~(reducer reduce-step body (:children n)) ))) )))

(defn- compile-clause
  [expr clauses]
    (condl
      (empty? clauses) [clauses nil]

      :let [[pat & remainder] clauses]
      (empty? remainder)
      (throw (IllegalArgumentException. (str "Pattern '" pat "' must be followed by a result or a guard")))

      (not= :if (first remainder))
      [(next remainder) (eval-node (parse-pat pat expr) [(first remainder)])]
      
      :let [[remainder guards]
            (loop [remainder remainder, guards []]   
              (condl
                (or (empty? remainder) (not= :if (first remainder)))
                [remainder guards]

                (count-bound 3 > remainder) 
                (throw (IllegalArgumentException. (str "expected (:if test result), got " remainder)))
		        
                :otherwise
                (recur (nthnext remainder 3) (conj guards [(nth remainder 1) (nth remainder 2)]) )))            
            compiled-guard
            (reducer (fn [[test result] inner]
                       (if (nil? inner)
                         `(if ~test [~result])
                         `(if ~test [~result] ~inner))
                       )
                     nil
                     guards)]
      :in
      [remainder (eval-node (parse-pat pat expr) compiled-guard)]   ))
      
              

(defmacro match-case
  "Pattern matching with local bindindgs, guards and results

  clause => clauses*
  clause => :case? pat result
  result => :then? expr , or guarded-result+
  guarded-result => :if test :then? expr

  pat => {map-pat*} , [pos-pat* rest-pat?] , 'quote-pat, (pred) , (fn [arg] test-body) , (:val expr)
         (:or pat+) , #\"re-pat\" , anything-pat ,   var-pat or const-pat
  map-pat => key pat ,        matches if expr is a map and pat matches (get expr key).
  pos-pat => pat ,            matches if expr is sequential with values matching every corresponding pat.
  rest-pat => & pat ,         matches the rest of a sequential expr.
  seq-pat => quote quote-pat ,or pred-pat ,or pred-fn-pat ,or :val v
  pred-pat => pred args*      matches if (pred arg* expr) is true.
  pred-fn-pat => fn [x] body* is shorthand for pred-pat ((fn[x] body*)), i.e. applies the predicate
  
  re-pat  =>                  matches strings.
  anything-pat => _      ,    equivalent to [but more efficient than] the pattern ((constantly true)).
  quote-pat  => any-form ,    literal match. 
  var-pat => symbol ,         matches anything, and binds to it over the result.
  const-pat => anything else, matches when = expr. Note that the pattern is not evaluated, but the expr is.

Example of everything:

 (match-case x  pat-w-result          'any-expr,
                pat-w-guarded-results :if (foo x) 'guarded-result,
                ; example of all pats
                                      :if (bar x) 'gr2,
                [ :a \"b\"         ; simple = match
                  3                ; simple == match
                  'any-form        ; simple= match    
                  _        ; anything
                  asym     ; anything, binding asym to match in result exprs
                  #\"re\"  ; what you'd expect
                  [x & xs :as y]
                  {:a av, keys [b c], syms [d], strs [c], :as m}
                  (:or p1 p2)
                  (:val expr)      ; = match with expr evaluated
                  (pred)           ; matches x if (pred x)
                  #(foo %)         ; matches x if (#(foo %) x) [same as ((foo %))]
                  (fn [y] (foo y)) ; same as above
                ] \"result\")
                

  TODO sets => match some,  first for sequential
  TODO :as pats
  TODO: (re-pat sym...) to bind groups to vars

  Note the following differences with regular destructing for maps:

  o the order is swapped so variable is on left:   { var pat }
  o directives are the bare symbols keys, strs and syms, rather than :keys, :strs and:syms

  :case and :then keywords are optional punctuation which might made code easier to read in some cases.  However,
  I haven't implemented it.
"
  [expr & clauses]
  (if (empty? clauses)
    nil
    (let [e (gensym "e")]
      `(first (let [~e ~expr]
                (or ~@(loop [clauses clauses, results []] 
                        (if (seq clauses)
                          (let [[clauses*, result] (compile-clause e clauses)]
                            (recur clauses* (conj results result)) )
                          results)))))) ))

