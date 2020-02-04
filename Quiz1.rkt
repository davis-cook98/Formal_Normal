#lang racket
(require fsm)

(define (alg dfa)
  (alg-helper dfa '(sm-getstart dfa) '(sm-getstart dfa)))

(define (alg-helper dfa state-list past-states)
  (letrec ([states-from-a (gets-to (car state-list))]
        [new-states (been-there past-states states-from-a)])
    (cond [(eqv? new-states past-states) (pretty-dfa dfa new-states)]
          [(null? state-list) (pretty-dfa dfa new-states)]
          [else (alg-helper dfa states-from-a new-states)])))
        
  

;gets-to: a-state + rules -> states
;Purpose: To see what states we can get to from a state
;state-a: input state
;rules: rules from the dfa
(define (gets-to state-a rules)
  (cond [(null? rules) '()]
        [(eqv? (caar rules) state-a) (cons (caddar rules) (gets-to state-a (cdr rules)))]
        [else (gets-to state-a (cdr rules))]))

(define (been-there past-states new-states)
  (if (member (car new-states) past-states) (been-there past-states (cdr new-states))
      (been-there (cons (car new-states) past-states) (cdr new-states))))

(define (pretty-dfa dfa reachable-states)
  (make-dfa
   reachable-states
   (sm-getalphabet dfa)
   (sm-getstart dfa)
   (sm-getfinals)
   (sm-getrules dfa)))