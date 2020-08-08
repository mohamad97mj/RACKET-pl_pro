#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct bool (boolean) #:transparent)
(struct plus  (e1 e2)  #:transparent)  ;; add two expressions
(struct minus  (e1 e2)  #:transparent)  ;; subtract two expressions
(struct mult  (e1 e2)  #:transparent)  ;; multiply two expressions
(struct div  (e1 e2)  #:transparent)  ;; divide two expressions
(struct neg  (e1)  #:transparent)  ;; negate an expressions
(struct andalso  (e1 e2)  #:transparent)  ;; logical conjunction
(struct orelse  (e1 e2)  #:transparent)  ;; logical disjunction
(struct cnd  (e1 e2 e3)  #:transparent)  ;; conditional
(struct iseq (e1 e2)  #:transparent)  ;; comparison
(struct ifnzero (e1 e2)  #:transparent)  ;; if not zero
(struct ifleq (e1 e2 e3 e4)  #:transparent)  ;; if less than or equal
;(struct lam (s1 s2 e)  #:transparent)  ;; lambda
(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application
;(struct myapply (funexp actual)  #:transparent) ;; function application
(struct apair (e1 e2)  #:transparent) ;; pair

(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false
(struct 1st (e1)   #:transparent) ;; the first part of the pair
(struct 2nd (e1)   #:transparent) ;; the second part of the pair
(struct ismunit (e)     #:transparent) ;; testing for munit
;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent)
(struct letrec (s1 e1 s2 e2 s3 e3 e4) #:transparent) ;; a letrec expression for recursive definitions
(struct queue (e q) #:transparent) ;; it holds several expressions
(struct enqueue (e q) #:transparent) ;; it enqueues e into q
(struct dequeue (q) #:transparent) ;; it dequeues q
(struct extract (q) #:transparent) ;; it returns queue's top element

;; Problem 1
(define (racketlist->numexlist xs)
  (cond [(null? xs) (munit)]
        [(list? xs) (apair (car xs) (racketlist->numexlist (cdr xs)))]
        [#t (error ("invalid racket list"))]
  )
)

(define (numexlist->racketlist xs)
  (cond [(munit? xs) null]
        [(apair? xs) (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs)))]
        [#t (error ("invalid numex list"))]
  )
)

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
  		"CHANGE" 
		)
 )

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        ;; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3) "CHANGE")

(define (with* bs e2) "CHANGE")

(define (ifneq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define numex-filter "CHANGE")

(define numex-all-gt
  (with "filter" numex-filter
        "CHANGE (notice filter is now in NUMEX scope)"))