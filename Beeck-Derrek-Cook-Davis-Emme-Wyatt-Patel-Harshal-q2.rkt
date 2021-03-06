#lang racket
(require test-engine/racket-tests)
(require fsm)
;; Quiz 3
;; Write a constructor for regular grammars that takes
;; as input a dfa. This constructor returns a regular
;; grammar that does not contain unproductive (silly) rules.


;; alg ndfa -> ndfa
;; Purpose: to change an ndfa to work for all prefixes of the language
(define (alg ndfa)
  (alg-helper ndfa (list (sm-getstart ndfa)) '()))

;; alg-helper2 ndfa child-states past-states -> ndfa
;; Purpose: To process an input ndfa recursively
;; child-states:list, states that have yet to have their children processed
;; past-states:list, states that we have visited previously
(define (alg-helper ndfa child-states past-states)
  (letrec ([children (gets-to (car child-states) (sm-getrules ndfa))]
           [new-states (filter (lambda (x) (member x children)) past-states)])
           ;[new-states (remove-duplicates (append past-states children))])
           ;[new-states (been-there past-states children)])
    (cond [(eqv? new-states past-states) (pretty-ndfa ndfa new-states)]
          ;The above cond is true when all of the states that we have found
          ;are equivalent to all of the states that we have visited,
          ;meaning that we have visited every reachable state. If we
          ;do not have this check, the algorithm will not terminate.
          [(null? child-states) (pretty-ndfa ndfa new-states)]
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


;; pretty- ndfa: ndfa reachable-states ndfa
;; Purpose: allowed for neat creation of a ndfa
;; reachable-states: list, reachable-states for the ndfa
(define (pretty-ndfa ndfa reachable-states)
  (make-ndfa
   (sm-getstates ndfa)
   (sm-getalphabet ndfa)
   (sm-getstart ndfa)
   (remove-duplicates (cons (sm-getstart ndfa)))
   (sm-getrules ndfa)))

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

(define a*a-fix (alg a*a))
(define a*prefix '(S F A))

(define mach-1-fix (alg mach-1))
(define 1-prefix '(S F))

(define mach-2-fix (alg mach-2))
(define 2-prefix '(Q0 Q1))

(check-expect (eqv?(sm-getfinals mach-2) 2-prefix) false)
(check-expect (sm-getfinals mach-2-fix) 2-prefix)

(check-expect (eqv?(sm-getfinals mach-1) 1-prefix) false)
(check-expect (sm-getfinals mach-1-fix) 1-prefix)

(check-expect (eqv?(sm-getfinals a*a) a*prefix) false)
(check-expect (sm-getfinals a*a-fix) a*prefix)

;been-there
(check-expect (been-there '(1 2 3) '(2 3 4))
              '(4 1 2 3))
(check-expect (been-there '(1 2 3) '(4 5 6))
              '(6 5 4 1 2 3))
(check-expect (been-there '(1 2 3) '(3 2 1))
              '(1 2 3))

;gets-to-final?
(check-expect (gets-to-final? (sm-getstart a*a-fix) (sm-getfinals a*a-fix) (sm-getrules a*a-fix)) true)
(check-expect (gets-to-final? '(Q3) (sm-getfinals mach-2) (sm-getrules mach-2)) false)
(check-expect (gets-to-final? (sm-getstart mach-2) (sm-getfinals mach-2) (sm-getrules mach-2)) true)

;gets-to
(check-expect (gets-to (sm-getstart a*a) (sm-getrules a*a)) '(F))
(check-expect (gets-to 'F (sm-getrules a*a)) '(F A))
(check-expect (gets-to 'A (sm-getrules mach-1)) '())
(check-expect (gets-to 'F (sm-getrules mach-1)) '(F S))
(check-expect (gets-to 'S (sm-getrules mach-1)) '(F S))

;are-final?
(check-expect (are-final? '(S F A) (sm-getfinals a*a) (sm-getrules a*a)) '(A F S))
(check-expect (are-final? '(Q0 Q1) (sm-getfinals mach-2) (sm-getrules mach-2)) '(Q1 Q0))
(check-expect (are-final? '(A F) (sm-getfinals mach-1) (sm-getrules mach-1))'(F))

(test)
