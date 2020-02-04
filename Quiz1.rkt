#lang racket
(require fsm)
(require test-engine/racket-tests)


(define (pretty-dfa dfa reachable-states)
  (make-dfa
   reachable-states
   (sm-getalphabet dfa)
   (sm-getstart dfa)
   (sm-getfinals)
   (sm-getrules dfa)))


(define (alg dfa)
  (alg-helper dfa (sm-getstart dfa) (sm-getstart dfa)))

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

(define mach-1
  (make-dfa '(Q0 Q1 Q3)
            '(a b)
            'Q0
            '(Q0 Q1)
            '((Q0 a Q1)
              (Q1 a Q0)
              (Q3 a Q3)
              (Q0 b Q0)
              (Q1 b Q1)
              (Q3 b Q3))))

(define mach-1-same
  (make-dfa '(Q0 Q1)
            '(a b)
            'Q0
            '(Q0 Q1)
            '((Q0 a Q1)
              (Q1 a Q0)
              (Q0 b Q0)
              (Q1 b Q1))))

(define mach-2
  (make-dfa '(Q0 Q1 Q2 Q3 Q4 Q5)
            '(a b)
            'Q1
            '(Q0 Q4 Q5)
            '((Q0 a Q5)
              (Q1 a Q2)
              (Q2 a Q3)
              (Q3 a Q4)
              (Q4 a Q4)
              (Q5 a Q0)
              (Q0 b Q5)
              (Q1 b Q3)
              (Q3 b Q2)
              (Q2 b Q4)
              (Q4 b Q4)
              (Q5 b Q0)
              )))


(define mach-2-same
  (make-dfa '(Q1 Q2 Q3 Q4)
            '(a b)
            'Q1
            '(Q4)
            '((Q1 a Q2)
              (Q2 a Q3)
              (Q3 a Q4)
              (Q4 a Q4)
              (Q1 b Q3)
              (Q3 b Q2)
              (Q2 b Q4)
              (Q4 b Q4))))

(define mach-3
  (make-dfa '(Q0 Q1 Q2 Q3 Q4 Q5)
            '(a b)
            'Q0
            '(Q5)
            '((Q0 a Q1)
              (Q1 a Q3)
              (Q3 a Q5)
              (Q5 a Q5)
              (Q2 a Q1)
              (Q4 a Q3)
              (Q0 b Q3)
              (Q3 b Q1)
              (Q1 b Q5)
              (Q5 b Q5)
              (Q2 b Q2)
              (Q4 b Q4))))

(define mach-3-same
  (make-dfa '(Q0 Q1 Q3 Q5)
            '(a b)
            'Q0
            '(Q5)
            '((Q0 a Q1)
              (Q1 a Q3)
              (Q3 a Q5)
              (Q5 a Q5)
              (Q0 b Q3)
              (Q3 b Q1)
              (Q1 b Q5)
              (Q5 b Q5))))


(check-expect (alg '((Q0 a Q1)
                                           (Q1 a Q0)
                                           (Q3 a Q3)
                                           (Q0 b Q0)
                                           (Q1 b Q1)
                                           (Q3 b Q3))) '((Q0 a Q1) (Q1 a Q0) (Q3 a Q3) (Q0 b Q0) (Q1 b Q1) (Q3 b Q3)))

(sm-testequiv? mach-1 mach-1-same)
(sm-testequiv? mach-2 mach-2-same)
(sm-testequiv? mach-3 mach-3-same)


(test)
