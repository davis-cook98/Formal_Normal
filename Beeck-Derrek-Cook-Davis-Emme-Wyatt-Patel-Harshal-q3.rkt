#lang racket
(require test-engine/racket-tests)
(require fsm)
;; Quiz 3
;; Write a constructor for regular grammars that takes as
;; input a dfa. This constructor returns a regular grammar
;; that does not contain unproductive (silly) rules.


;; alg ndfa -> ndfa
;; Purpose: to change an ndfa to work for all prefixes of the language
(define (alg ndfa)
  (alg-helper ndfa (list (sm-getstart ndfa)) '()))

;; alg-helper ndfa child-states past-states -> ndfa
;; Purpose: To process an input ndfa recursively
;; child-states:list, states that have yet to have their children processed
;; past-states:list, states that we have visited previously
(define (alg-helper ndfa child-states past-states)
  (letrec ([children (gets-to (car child-states) (sm-getrules ndfa))]
           [new-states (been-there past-states children)])
    (cond [(eqv? new-states past-states) (pretty-ndfa ndfa (remove-states new-states (sm-getfinals ndfa) (sm-getrules ndfa)))]
          ;The above cond is true when all of the states that we have found
          ;are equivalent to all of the states that we have visited,
          ;meaning that we have visited every reachable state. If we
          ;do not have this check, the algorithm will not terminate.
          [(null? child-states) (pretty-ndfa ndfa (remove-states new-states (sm-getfinals ndfa) (sm-getrules ndfa)))]
          [else (alg-helper ndfa children new-states)])))

;; gets-to: a-state + rules -> states
;; Purpose: To see what states we can get to from a state
;; state-a: input state
;; rules:list, rules from the ndfa
(define (gets-to state-a rules)
  (cond [(null? rules) '()]
        [(eqv? (caar rules) state-a) (cons (caddar rules) (gets-to state-a (cdr rules)))]
        [else (gets-to state-a (cdr rules))]))

;; been-there: past-states new-states -> (union past-states new-states)
;; Purpose: to determine what states we have been to before and which
;; ones we have not. This allows the function to have "memory" and
;; prevent going in a loop.
;; past-states:list, states that we have visited previously
;; new-states:list, states that we have visited in the last child state we checked
(define (been-there past-states new-states)
  (cond [(null? new-states) past-states]
        [(member (car new-states) past-states) (been-there past-states (cdr new-states))]
        [else (been-there (cons (car new-states) past-states) (cdr new-states))]))

;; gets-to-final?: start-state final-states rules -> boolean
;; Purpose: to determine if a reachable state can get to a final state
(define (gets-to-final? start-state final-states rules)
  (cond [(null? rules) false]
        [(and (eqv? (caar rules) start-state)
              (member (caddar rules) final-states)) true]
        [else (gets-to-final? start-state final-states (cdr rules))]))

;; Purpose: remove reachable states that cannot reach the final state
(define (remove-states reachable-states final-states rules)
  (cond [(null? reachable-states) '()]
        [(gets-to-final? (car reachable-states) final-states rules) (cons (car reachable-states) (remove-states (cdr reachable-states) final-states rules))]
        [else (remove-states (cdr reachable-states) final-states rules)]))

;; Purpose: remove silly rules
(define (remove-rules reachable-states rules)
  (cond [(null? rules) '()]
        [(or (not (member (caar rules) reachable-states))
             (not (member (caddar rules) reachable-states))) (remove-rules reachable-states (cdr rules))]
        [else (cons (car rules) (remove-rules reachable-states (cdr rules)))]))

;; pretty- ndfa: ndfa reachable-states ndfa
;; Purpose: allowed for neat creation of a ndfa
;; reachable-states: list, reachable-states for the ndfa
(define (pretty-ndfa ndfa reachable-states)
  (make-ndfa
   (remove-duplicates (cons (sm-getstart ndfa) reachable-states))
   (sm-getalphabet ndfa)
   (sm-getstart ndfa)
   (sm-getfinals ndfa)
   (remove-rules reachable-states (sm-getrules ndfa))))

;; TESTS!!!!!!
(define a*a (make-ndfa '(S F A)       ;the states
                       '(a b)         ;the alphabet
                       'S             ;the starting state
                       '(F)           ;final states
                       '((S a F)      ;the transition function
                         (F a F)
                         (F b A)
                         (A a F)
                         (A b A))))

(define mach-1 (make-ndfa '(S F A)
                          '(a b c)
                          'S
                          '(F)
                          '((S a F)
                            (F b F)
                            (S c S)
                            (F c S))))

(define mach-2 (make-ndfa '(Q0 Q1 Q2 Q3)
                          '(a b)
                          'Q0
                          '(Q0 Q1)
                          '((Q0 a Q0)
                            (Q0 b Q1)
                            (Q1 a Q1)
                            (Q1 b Q0)
                            (Q2 a Q3)
                            (Q3 b Q2)
                            (Q2 b Q3)
                            (Q3 a Q2))))

(define mach-2-fix (alg mach-2))

(test)
