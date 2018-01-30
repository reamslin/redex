#lang racket/base

;; The module implements a render-derivation function
;; that produces a pict of the tree of the derivation

(provide
 ;; render-derivation: language derivation -> pict
 render-derivation)

;; Import and Implementation

(require redex
         pict
         racket/list
         racket/match)

;; render-derivation: language derivation -> pict
;; return a pict of the tree of the derivation 
(define (render-derivation lang der)
    (match-define (derivation term name subs) der)
    (define conclusion-pict (render-term/pretty-write lang term))
    (define predeces-pict
      (apply hb-append (map (λ (v) (render-derivation lang v)) subs)))
    (define line-width 3)
    (define space-before-name 10)
    (define name-pict (hc-append (blank space-before-name 0) (text (string-append "[" name "]"))))
    (define offset-r (pict-width name-pict))
    (vc-append predeces-pict
               (hc-append (blank offset-r 0)
                          (filled-rectangle
                           (max (pict-width conclusion-pict)
                                (pict-width predeces-pict))
                           line-width)
                          name-pict)
               conclusion-pict))

(module+ test
  
  (define-language nats
    (n ::= z
       (s n)))

  (define-judgment-form nats
    #:mode (sum I I O)
    #:contract (sum n n n)
    [--------------  "zero"
     (sum z n n)]

    [(sum n_1 n_2 n_3)
     ---------------------------- "add1"
     (sum (s n_1) n_2 (s n_3))])

  (define example1 (render-derivation nats
                                      (first
                                       (build-derivations
                                        (sum (s (s (s z))) (s z) (s (s (s (s z)))))))))
  
  (define-judgment-form nats
    #:mode (even I)
    #:contract (even n)

    [------------- "evenz"
     (even z)]

    [(even n)
     ------------- "even2"
     (even (s (s n)))])

  (define-judgment-form nats
    #:mode (all-even I)
    #:contract (all-even (n ...))
    [(even n) ...
     ----------------- "all-even"
     (all-even (n ...))])

  (define (make-example2)
    (render-derivation nats
                       (first
                        (build-derivations
                         (all-even ((s (s z)) z (s (s (s (s z))))))))))

  (define example3
    (with-atomic-rewriter
        'z
      "zero"
      (make-example2)))

  (show-pict example1)
  (show-pict (make-example2))

  (require racket/pretty)
  (parameterize ([pretty-print-columns 10])
    (show-pict (make-example2)))
  
  (show-pict example3))

