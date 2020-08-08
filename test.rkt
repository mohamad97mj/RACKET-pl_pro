#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

(define atom? (lambda (x)
                (cond ((pair? x) false)
                      ((null? x) false)
                      (true true))))


(atom? 1)
(atom? '1)