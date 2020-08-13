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
(struct ifnzero (e1 e2 e3)  #:transparent)  ;; if not zero
(struct ifleq (e1 e2 e3 e4)  #:transparent)  ;; if less than or equal
(struct with (s e1 e2)  #:transparent) ;; a let expression where the value of e1 is bound to s in e2
(struct lam (s1 s2 e)  #:transparent)  ;; lambda
(struct apply (e1 e2)       #:transparent) ;; function application
;(struct myapply (funexp actual)  #:transparent) ;; function application
(struct apair (e1 e2)  #:transparent) ;; pair

(struct 1st (e)   #:transparent) ;; the first part of the pair
(struct 2nd (e)   #:transparent) ;; the second part of the pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false
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
  (cond [(null? env) (error "unbound variable during evaluation" str)])
  (cond [(equal? str (car (car env))) (cdr (car env))]
        [else (envlookup (cdr env) str)]
  )
)

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(munit? e) e]


        [(num? e)
            (cond [(integer? (num-int e)) e]
                       [else (error "NUMEX num must be a racket integer")])]

        [(bool? e)
                    (cond [(boolean? (bool-boolean e)) e]
                               [else (error "NUMEX bool must be a racket boolean")])]

        [(plus? e)
            (let ([v1 (eval-under-env (plus-e1 e) env)]
                  [v2 (eval-under-env (plus-e2 e) env)])
                (if (and (num? v1) (num? v2))
                   (num (+ (num-int v1) (num-int v2)))
                   (error "NUMEX plus applied to non-num")))]

        [(minus? e)
            (let ([v1 (eval-under-env (minus-e1 e) env)]
                  [v2 (eval-under-env (minus-e2 e) env)])
                (if (and (num? v1) (num? v2))
                   (num (- (num-int v1) (num-int v2)))
                   (error "NUMEX minus applied to non-num")))]

        [(mult? e)
            (let ([v1 (eval-under-env (mult-e1 e) env)]
                  [v2 (eval-under-env (mult-e2 e) env)])
                (if (and (num? v1) (num? v2))
                   (num (* (num-int v1) (num-int v2)))
                   (error "NUMEX mult applied to non-num")))]

        [(div? e)
            (let ([v1 (eval-under-env (div-e1 e) env)]
                  [v2 (eval-under-env (div-e2 e) env)])
                (if (and (num? v1) (num? v2))
;                   (if (equal? (num-int v2) 0) (bool #f) (num (quotient (num-int v1) (num-int v2))))
                   (num (quotient (num-int v1) (num-int v2)))
                   (error "NUMEX div applied to non-num")))]

        [(neg? e)
            (let ([v1 (eval-under-env (neg-e1 e) env)])
                (cond
                    [(num? v1) (num (- (num-int v1)))]
                    [(bool? v1) (bool (not (bool-boolean v1)))]
                    [#t (error "NUMEX negation applied to non-num")]))]

        [(andalso? e)
            (let ([v1 (eval-under-env (andalso-e1 e) env)])
                  (if (equal? #f (bool-boolean v1)) v1
                       (let ([v2 (eval-under-env (andalso-e2 e) env)])
                            (if (and (or (bool? v1) (num? v1))(or (bool? v2) (num? v2)))
                                (cond
                                    [(and (bool? v1) (bool? v2)) (
                                      bool (and (bool-boolean v1) (bool-boolean v2))
                                    )]
                                    [(and (bool? v1) (num? v2)) (if (bool-boolean v1) v2 v1)]
                                    [(num? v1) v2]
                                 )
                                (error "NUMEX andalso applied to non-bool")))))]

        [(orelse? e)
            (let ([v1 (eval-under-env (orelse-e1 e) env)]
                  [v2 (eval-under-env (orelse-e2 e) env)])
                    (if (and (or (bool? v1) (num? v1))(or (bool? v2) (num? v2)))
                        (cond
                            [(and (bool? v1) (bool? v2)) (
                              bool (or (bool-boolean v1) (bool-boolean v2))
                            )]
                            [(and (bool? v1) (num? v2)) (if (bool-boolean v1) v1 v2)]
                            [(num? v1) v1]
                         )
                        (error "NUMEX orelse applied to non-bool")))]

        [(iseq? e)
            (let ([v1 (eval-under-env (iseq-e1 e) env)]
                  [v2 (eval-under-env (iseq-e2 e) env)])
                    (if (and (or (bool? v1) (num? v1))(or (bool? v2) (num? v2)))
                        (cond
                            [(and (bool? v1) (bool? v2))
                            (if (equal? (bool-boolean v1) (bool-boolean v2)) (bool #t) (bool #f))]
                            [(and (num? v1) (num? v2))
                            (if (equal? (num-int v1) (num-int v2)) (bool #t) (bool #f))]
                            [#t (bool #f)]
                        )
                        (error "NUMEX iseq applied to non-bool non-num")))]

        [(ismunit? e)
            (let ([v (eval-under-env (ismunit-e e) env)])
;               (if (munit? v) (num 1) (num 0)))] this line is corrcect based on project definition but in test it fails!
               (if (munit? v) (bool #t) (bool #f)))]


        [(apair? e)
            (let ([v1 (eval-under-env (apair-e1 e) env)]
                  [v2 (eval-under-env (apair-e2 e) env)])
                    (apair v1 v2))]

        [(cnd? e)
            (let ([v (eval-under-env (cnd-e1 e) env)])
                (if (bool? v)
                  (if (bool-boolean v)
                    (eval-under-env (cnd-e2 e) env)
                    (eval-under-env (cnd-e3 e) env))
                (error "NUMEX cnd first argument must be a bool")))]

        [(ifnzero? e)
            (let ([v (eval-under-env (ifnzero-e1 e) env)])
                (if (num? v)
                  (if (equal? v (num 0))
                    (eval-under-env (ifnzero-e3 e) env)
                    (eval-under-env (ifnzero-e2 e) env))
                (error "NUMEX ifnezro 1st argument must be a num")))]

        [(1st? e)
            (let ([v (eval-under-env (1st-e e) env)])
               (cond
                 [(apair? v) (apair-e1 v)]
                 [true (error "Numex first argument must be a pair")]))]

        [(2nd? e)
            (let ([v (eval-under-env (2nd-e e) env)])
               (cond
                 [(apair? v) (apair-e2 v)]
                 [true (error "Numex second argument must be a pair")]))]

        [(closure? e) e]

        [(lam? e) (closure env e)]

        [(apply? e)
            (let ([clsr (eval-under-env (apply-e1 e) env)])
                (cond
                  [(closure? clsr) (let ([lamDef (closure-f clsr)])
                                           (let ([arg (eval-under-env (apply-e2 e) env)])
                                             (eval-under-env (lam-e lamDef)
                                                (cons
                                                  (cons (lam-s2 lamDef) arg)
                                                  (cons
                                                    (cons (lam-s1 lamDef) clsr)
                                                    (closure-env clsr))))))]
                  [#t (error "Numex apply first argument must be a closure")]))]

        [(ifleq? e)
            (let ([v1 (eval-under-env (ifleq-e1 e) env)]
                  [v2 (eval-under-env (ifleq-e2 e) env)])
                    (if (and (num? v1) (num? v2))
                      (if (> (num-int v1) (num-int v2))
                        (eval-under-env (ifleq-e4 e) env)
                        (eval-under-env (ifleq-e3 e) env))
                    (error "NUMEX ifleq 1st and 2nd argument must be nums")))]

        [(with? e)
             (let ([s (with-s e)]
                   [v (eval-under-env (with-e1 e) env)])
                    (eval-under-env (with-e2 e) (cons (cons s v) env)))]




        [#t (error "bad NUMEX expression: ~v" e)]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
(define (with* bs e2)
  (if (equal? bs null)
      e2
      (with (car (car bs)) (cdr (car bs)) (with* (cdr bs) e2))))


(define (ifneq e1 e2 e3 e4)
    (cnd (iseq e1 e2)
         e4
         e3))

(define (ifmunit e1 e2 e3)
    (cnd (ismunit e1)
         e2
         e3))


;
;;; Problem 4
;
(define numex-filter (lam "self" "lam_arg" (lam "filter" "list"
    (cnd (ismunit (var "list"))
        (munit)
        (apair (apply (var "lam_arg") (1st (var "list"))) (apply (var "filter") (2nd(var "list"))))))))


;(define numex-all-gt
;  (with "filter" numex-filter
;        "CHANGE (notice filter is now in NUMEX scope)"))

(define numex-all-gt (lam null "i" (lam null "someList" (apply (apply numex-filter (lam "addition"  "x" (plus (var "x") (var "i")))) (var "someList")))))
