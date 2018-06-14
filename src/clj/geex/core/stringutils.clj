(ns geex.core.stringutils)

(def compact {:prefix " " :step ""})

;; Convention: Whenever risk of ambiguity,
;; a function should wrap its output in parenthesis.
;; But it is not its responsibility to wrap its input.
(defn wrap-in-parens [x]
  [compact  "(" x ")"])

