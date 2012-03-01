(ns mgm7734.test.util
  (:use [clojure.test]
        [mgm7734.util]))

(def ^:dynamic cnt)


( deftest match-case-test
	 (binding [cnt 0]
	  (defn expensive [] 
	    (set! cnt (inc cnt)) 
	    {:a 1, :b 2})
	  (is (match-case (expensive), {:a 1, :b 2} true))
	  (is (= 1 cnt))
	  
	  (is (match-case (expensive), [1 2 3] false, {:a 1, :b 2} :if false false,  {:a 1, :b 2} true))
	  (is (= 2 cnt))
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

;(match-case [1 2], 
;   :case [x] :if (odd? x)  :then "odd singleton",
;             :if (even? x) :then "even singleton"
;   :case [x y] :then "any pair" )
  )