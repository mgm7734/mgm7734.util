(ns mgm7734.test.util
  (:use [clojure.test]
        [mgm7734.util]))

(def ^:dynamic cnt)

(deftest match?-tests
	(is      (match? 1 1))
	(is (not (match? 0                1)))
	(is      (match? '(anything quoted) '(anything quoted)))       
	(is (not (match? {:a 0}           {:a 1 :b 2})))
	(is      (match? {:a (even?)}     {:a 2}))
	(is (not (match? {:a (even?)}     {:a 1})))
	(is      (match? _                {:foo '(will match? anything)}))
	(is      (match? [(even?) 1]      [2 1])) 
	(is (not (match? [(even?) 1]      [2 1 0])))
	(is      (match? [(even?) 1 & _]  [2 1 0 -1])) 
	
	(is      (match? #"H.*!"          "Hello, world!"))
   
;   (is (= :ok (try (macroexpand '(match? a 1)) (catch IllegalArgumentException e :ok))))

(binding [cnt 0]
  (defn expensive [] 
    (set! cnt (inc cnt)) 
    {:a 1, :b 2})
  (is (match? {:a 1, :b 2} (expensive)))
  (is (= 1 cnt)))

(binding [cnt 0]
  (defn expensive [] 
    (set! cnt (inc cnt)) 
    [1 2 3])  
  (is (match? [1 2 3] (expensive)))
  (is (= 1 cnt)))
)

(deftest match-map-test
   (is (= {:a 3} (match-map a                  (+ 1 2))))
   (is (= {}     (match-map 1                  1)))
   (is (not      (match-map 0                  1)))
	(is (= {}     (match-map '(anything quoted) '(anything quoted))))       
	(is (not      (match-map {:a 0}             {:a 1 :b 2})))
	(is (= {}     (match-map {:a (even?)}       {:a 2})))
	(is (not      (match-map {:a (even?)}       {:a 1})))
	(is (= {}     (match-map _                  {:foo '(will match? anything)})))
	(is (= {:x 1} (match-map [(even?) x]        [2 1]))) 
	(is (not      (match-map [(even?) x]        [2 1 0]))) 
	(is (= {:x 1 :y [0 -1]} (match-map [(even?) x & y]    [2 1 0 -1]))) 
	(is (= {}     (match-map #"H.*!"          "Hello, world!")))
  
 (binding [cnt 0]
  (defn expensive [] 
    (set! cnt (inc cnt)) 
    {:a 1, :b 2})
  (is (match-map {:a 1, :b 2} (expensive)))
  (is (= 1 cnt)))

(binding [cnt 0]
  (defn expensive [] 
    (set! cnt (inc cnt)) 
    [1 2 3])  
  (is (match-map [1 2 3] (expensive)))
  (is (= 1 cnt)))

  (is (= {:a 1, :b 2, :c 3, :d 4, :e 5, :f  6, :g 7} 
         (match-map {keys [a b] strs [c d] syms [e f] :g g}
                    {:a 1, :b 2, "c" 3, "d" 4, 'e 5, 'f 6, :g 7})))
  )

( deftest match-case-test
  )
	(is (= :ok (match-case 1     1  :ok)))
	(is (= nil (match-case 0     1  :ok)))
	(is (= :ok (match-case 'a    'a :ok)))
	(is (= nil (match-case 'a    'b :ok)))
	(is (= :ok (match-case [:ok]  a (first a))))
	
	(is (= 2 (match-case   [1 [2]]  , [1 x]  (first x)))) 
	(is (= nil (match-case [1 [2] 3], [1 x]  (first x)))) 
	(is (= [:ok :fine] (match-case [1 2 :ok :fine], [1 _ & y]   y)))

(is (not   (match-case {:a 1 :b 2},      {:a 2}   true)))
(is (= :ok (match-case {:a 1 :b 2 :c 3}, {:b 2}   :ok)))
(is (= [1 2 3 4 5 6 7] 
       (match-case {:a 1,:b 2,  "c" 3, "d" 4,  'e 5, 'f 6, :g 7, :h 8 :i 9}, 
                   {keys[a b],  strs[c d],     syms[e f]   :g g :h (even?)}
                   [a b c d e f g])))

(is (= :ok (match-case "Hello, world!", #"H.*!" :ok)))
(is (not   (match-case "Hello, world!", #"H.*X" :ok)))

(is (= :ok (match-case '(this is literal), '(this is literal) :ok )))

(is (= :ok (match-case :oh,    (:or :oh  [1 2] )  :ok)))
(is (= :ok (match-case [1 2],  (:or :oh  [1 2] ) :ok)))
(is (not (match-case   :no,    (:or :oh  [1 2] ) :ouch)))

(is (= :ok (match-case :a,  :b :ouch, :a :ok, :c :ouch)))
(is (= :ok (match-case :x,  :a :ouch, :x :if false :ouch, :if true :ok)))
(is (= :ok (match-case :x,  :a :if false :ouch, :x :ok)))