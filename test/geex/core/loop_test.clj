(ns geex.core.loop-test
  (:require [clojure.test :refer :all]
            [geex.core.loop :refer :all]))

(deftest loop-arg-parse-test
  (is (= (parse-loop-args [['a 3 'b 4] '(+ a b c)])
         {:bindings [{:vars 'a, :expr 3}
                     {:vars 'b, :expr 4}],
          :body '((+ a b c))})))
