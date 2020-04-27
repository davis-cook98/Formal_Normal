#lang Racket
(require fsm)

;; Created by fsm-GUI on: 4/2020 at 3:34pm
;; This machine passed all tests.
(define P49 (make-tm (quote (S A H)) (quote (a b @)) (quote (((A _) (S R)) ((A b) (S R)) ((A a) (H a)) ((S _) (S R)) ((S b) (S R)) ((S a) (A R)) ((A @) (A R)) ((S @) (S R)))) (quote S) (quote (H))))
