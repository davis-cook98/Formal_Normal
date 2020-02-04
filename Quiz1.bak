#lang racket
(require test-engine/racket-tests)
(require fsm)
;; Quiz 1 Formal Languages: Question 2.1.7
;;
;; We say that state q of a deterministic finite automaton M = (K, ~,J, qo, F)
;; is reachable if there exists W E ~* such that (qo, w) f-M (q, e). Show that
;; if we delete from NI any nonreachable state, an automaton results that
;; accepts the same language. Give an efficient algorithm for computing the
;; set of all reachable states of a deterministic finite automaton.

;; Proof by Induction 

;; alg dfa -> dfa
;; Purpose: to remove unreachable states and rules from a dfa
(define (alg dfa)
  (alg-helper dfa (sm-getstart dfa)))

;; alg-helper dfa start-state -> dfa
;; Purpose: First step in the process, gets initial list of
;; children. Needed because we cannot get first of a symbol
;; start-state: start state for the given dfa
(define (alg-helper dfa start-state)
  (let ([children (gets-to start-state (sm-getrules dfa))])
    (alg-helper2 dfa children '())))

;; alg-helper2 dfa child-states past-states -> dfa
;; Purpose: To process the states that alg-helper could
;; not recursively
;; child-states:list, states that have yet to have their children processed
;; past-states:list, states that we have visited previously
(define (alg-helper2 dfa child-states past-states)
  (letrec ([children (gets-to (car child-states) (sm-getrules dfa))]
           [new-states (been-there past-states children)])
    (cond [(eqv? new-states past-states) (pretty-dfa dfa new-states)]
          [(null? child-states) (pretty-dfa dfa new-states)]
          [else (alg-helper2 dfa children new-states)])))

;; gets-to: a-state + rules -> states
;; Purpose: To see what states we can get to from a state
;; state-a: input state
;; rules:list, rules from the dfa
(define (gets-to state-a rules)
  (cond [(null? rules) state-a]
        [(eqv? (caar rules) state-a) (cons (caddar rules) (gets-to state-a (cdr rules)))]
        [else (gets-to state-a (cdr rules))]))


;; been-there: past-states new-states -> (union past-states new-states)
;; Purpose: to determine what states we have been to before and which
;; ones we have not. This allows the function to have "memory" and
;; prevent going in a loop.
;; past-states:list, states that we have visited previously
;; new-states:list, states that we have visited in the last child state we checked
(define (been-there past-states new-states)
  (if (symbol? new-states)
      (cond [(null? new-states) past-states]
            [(member new-states past-states) past-states]
            [else (been-there (cons new-states past-states))])
      (cond [(null? new-states) past-states]
            [(member (car new-states) past-states) (been-there past-states (cdr new-states))]
            [else (been-there (cons (car new-states) past-states) (cdr new-states))])))

;; remove-rules: rules reachable-states -> rules
;; Purpose: to remove the rules that led to unreachable states.
;; rules:list, rules for the original dfa
;; reachable-states:list, reachable states for the dfa
(define (remove-rules rules reachable-states)
  (cond [(eqv? (caar rules) 'ds) '()]
        [(and (member (caddar rules) reachable-states)
              (member (caar rules) reachable-states))
         (cons (car rules) (remove-rules (cdr rules) reachable-states))]
        [else (remove-rules (cdr rules) reachable-states)]))

;; pretty-dfa: dfa reachable-states dfa
;; Purpose: allowed for neat creation of a dfa
;; reachable-states: list, reachable-states for the dfa
(define (pretty-dfa dfa reachable-states)
  (make-dfa
   reachable-states
   (sm-getalphabet dfa)
   (sm-getstart dfa)
   (sm-getfinals dfa)
   (remove-rules (sm-getrules dfa) reachable-states)))

;; TESTS!!!!!!
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

(sm-testequiv? mach-2-fix mach-2)
(sm-testequiv? mach-1-fix mach-1)

(check-expect (been-there '(1 2 3) '(2 3 4))
              '(4 1 2 3))
(check-expect (been-there '(1 2 3) '(4 5 6))
              '(6 5 4 1 2 3))
(check-expect (been-there '(1 2 3) '(3 2 1))
              '(1 2 3))

(check-expect (remove-rules
               (sm-getrules mach-2)
               '(Q0 Q1))
              '((Q0 a Q0)
              (Q0 b Q1)
              (Q1 a Q1)
              (Q1 b Q0)))
(check-expect (remove-rules
               (sm-getrules mach-2)
               '(Q0))
              '((Q0 a Q0)))
(check-expect (remove-rules
               (sm-getrules mach-1)
               '(Q0 Q1))
              '((Q0 a Q0)
              (Q0 b Q1)
              (Q1 a Q1)
              (Q1 b Q0)))

(test)
