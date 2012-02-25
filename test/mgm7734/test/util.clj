(ns mgm7734.test.util
  (:use [clojure.test]
        [mgm7734.util]))

(deftest match?-tests
	(is      (match? 1 1))
	(is (not (match? 0                1)))
	(is      (match? '(anything quoted) '(anything quoted)))       
	(is (not (match? {:a 0}           {:a 1 :b 2})))
	(is      (match? {:a (even?)}     {:a 2}))
	(is (not (match? {:a (even?)}     {:a 1})))
	(is      (match? :_               {:foo '(will match? anything)}))
	(is      (match? [(even?) 1]      [2 1])) 
	(is (not (match? [(even?) 1]      [2 1 0])))
	(is      (match? [(even?) 1 & :_] [2 1 0 -1])) 
	
	(is      (match? #"H.*!"          "Hello, world!"))
   
;   (is (= :ok (try (macroexpand '(match? a 1)) (catch IllegalArgumentException e :ok))))
  )
