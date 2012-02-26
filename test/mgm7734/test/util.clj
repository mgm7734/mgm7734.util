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

   (is (= ['a 3] (match-binds a                  (+ 1 2))))
   (is (= []     (match-binds 1                  1)))
   (is (not      (match-binds 0                  1)))
	(is (= []     (match-binds '(anything quoted) '(anything quoted))))       
	(is (not      (match-binds {:a 0}             {:a 1 :b 2})))
	(is (= []     (match-binds {:a (even?)}       {:a 2})))
	(is (not      (match-binds {:a (even?)}       {:a 1})))
	(is (= []     (match-binds _                  {:foo '(will match? anything)})))
	(is (= ['x 1] (match-binds [(even?) x]        [2 1]))) 
	(is (not      (match-binds [(even?) x]        [2 1 0]))) 
	(is (= ['x 1 'y [0 -1]] (match-binds [(even?) x & y]    [2 1 0 -1]))) 
	(is (= []     (match-binds #"H.*!"          "Hello, world!")))
  
 (binding [cnt 0]
  (defn expensive [] 
    (set! cnt (inc cnt)) 
    {:a 1, :b 2})
  (is (match-binds {:a 1, :b 2} (expensive)))
  (is (= 1 cnt)))

(binding [cnt 0]
  (defn expensive [] 
    (set! cnt (inc cnt)) 
    [1 2 3])  
  (is (match-binds [1 2 3] (expensive)))
  (is (= 1 cnt)))
)