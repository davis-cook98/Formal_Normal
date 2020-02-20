#lang racket
(require fsm)

;;ndfa -> ndfa
(define (machine-prefix m1)
  (let* ((m1-start (sm-getstart m1));;gets the states from the original machine
         (m1-final (sm-getfinals m1));gets the final state
         (m1-singleFinal (car m1-final));;gets only the first of the final states 
         (m1-sigma (sm-getalphabet m1));;gets the alphabet
         (m1-states (sm-getstates m1));gets the states
         (m1-rules (sm-getrules m1));;gets the rules of the ndfa
         (m1-dup-remover (remove-duplicates m1-rules));;removes any duplicates
       )
    (make-ndfa
     m1-states
     m1-sigma
     m1-start
     m1-final
     m1-rules)))


(define KLEENESTAR-abUaba (make-ndfa '(Q-0 Q-1 Q-2 Q-3 Q-4 Q-5 Q-6) ;the states
                                      '(a b)                    ;the alphabet
                                     'Q-0                       ;the starting state
                                     '(Q-6)                     ;the final states
                                     `((Q-0 a Q-1)              ;the transition relation
                                       (Q-1 b Q-2)
                                       (Q-2 a Q-3)
                                       (Q-3 ,EMP Q-0)
                                       (Q-0 a Q-4)
                                       (Q-4 b Q-5)
                                       (Q-5 ,EMP Q-0))))