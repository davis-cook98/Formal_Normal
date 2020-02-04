#lang racket
(require fsm)

(define (alg dfa)
  (alg-helper dfa (sm-getstart dfa)))

(define (alg-helper dfa start-state)
  (let ([children (gets-to start-state (sm-getrules dfa))])
    (alg-helper2 dfa children '())))


(define (alg-helper2 dfa child-states past-states)
  (letrec ([children (gets-to (car child-states) (sm-getrules dfa))]
           [new-states (been-there past-states children)])
    (cond [(eqv? new-states past-states) (pretty-dfa dfa new-states)]
          [(null? child-states) (pretty-dfa dfa new-states)]
          [else (alg-helper2 dfa children new-states)])))

;gets-to: a-state + rules -> states
;Purpose: To see what states we can get to from a state
;state-a: input state
;rules: rules from the dfa
(define (gets-to state-a rules)
  (cond [(null? rules) state-a]
        [(eqv? (caar rules) state-a) (cons (caddar rules) (gets-to state-a (cdr rules)))]
        [else (gets-to state-a (cdr rules))]))

(define (been-there past-states new-states)
  (if (eqv? 'Q0 new-states)
      (cond [(null? new-states) past-states]
        [(member new-states past-states) past-states]
        [else (been-there (cons new-states past-states))])
  (cond [(null? new-states) past-states]
        [(member (car new-states) past-states) (been-there past-states (cdr new-states))]
        [else (been-there (cons (car new-states) past-states) (cdr new-states))])))

(define (remove-rules rules final-states)
  (if (and (member (caddar rules) final-states)
           (member (caar rules) final-states)) (cons (car rules) (remove-rules (cdr rules) final-states))
                                               (remove-rules (cdr rules) final-states)))

(define (pretty-dfa dfa reachable-states)
  (make-dfa
   reachable-states
   (sm-getalphabet dfa)
   (sm-getstart dfa)
   (sm-getfinals dfa)
   ;(remove-rules (sm-getrules dfa) reachable-states)))
   (sm-getrules dfa)))


(define mach-1
  (make-dfa '(Q0 Q1)
            '(a b)
            'Q0
            '(Q0 Q1)
            '((Q0 a Q0)
              (Q0 b Q1)
              (Q1 a Q1)
              (Q1 b Q0))))

(define mach-2
  (make-dfa '(Q0 Q1 Q2 Q3)
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
(define mach-1-fix (alg mach-1))

(sm-testequiv? mach-2-fix mach-1)
(sm-testequiv? mach-1-fix mach-1)
