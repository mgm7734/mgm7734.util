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
         (match-map {keys> [a b] strs> [c d] syms> [e f] :g g}
                    {:a 1, :b 2, "c" 3, "d" 4, 'e 5, 'f 6, :g 7})))
)
