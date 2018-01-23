#lang racket

(require redex)
(require pict)

; render-derivation: language derivation -> pict
; return a pict of the tree of the derivation
(define (render-derivation lang der)
  (define term (derivation-term der))
  (define name (derivation-name der))
  (define subs (derivation-subs der))
  (define conclusion-pict (render-term/pretty-write lang term))
  (define predeces-pict (foldr (λ (v l)
                                 (hc-append
                                  (render-derivation lang v)
                                  l))
                               (blank) subs))
  (define line-width 5)
  (define name-pict (hc-append (blank 10 0) (text name)))
  (define offset-r (pict-width name-pict))
  (vc-append predeces-pict
             (hc-append (blank offset-r 0)
                        (hline
                         (max (pict-width conclusion-pict)
                              (pict-width predeces-pict))
                         line-width)
                        name-pict)
             conclusion-pict))

; Examples

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

(define example2 (render-derivation nats
                                    (first
                                     (build-derivations
                                      (all-even ((s (s z)) z (s (s (s (s z))))))))))

