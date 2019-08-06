#lang racket/base
(require "provide.rkt"
         "misc.rkt"
         "arr-i.rkt"
         "base.rkt"
         "arrow-val-first.rkt"
         syntax/location
         racket/stxparam
         (for-syntax racket/base))

(provide define-region)

(define-syntax (define-region stx)
  (syntax-case stx ()
    [(_ (f arg ...) . more)
     (let ()
       (define-values (arg-ids ->i-doms)
         (for/fold ([arg-ids '()] [->i-doms '()])
                   ([arg (in-list (syntax->list #'(arg ...)))])
           (syntax-case arg ()
             [(x ctc)
              (identifier? #'x)
              (values (cons #'x arg-ids) (cons arg ->i-doms))]
             [(x (dep-x ...) ctc)
              (identifier? #'x)
              (let ()
                (for ([dep-x (in-list (syntax->list #'(dep-x ...)))])
                  (unless (identifier? dep-x)
                    (raise-syntax-error
                     #f
                     "expected an identifier as the dependency"
                     stx dep-x)))
                (values (cons #'x arg-ids)
                        (cons arg ->i-doms)))]
             [_
              (raise-syntax-error
               #f
               "malformed argument"
               stx arg)])))

       (define-values (rng bodies)
         (syntax-case #'more ()
           [(#:range ctc body1 body2 ...)
            (values #'ctc #'(body1 body2 ...))]
           [(#:range ctc)
            (raise-syntax-error
             #f
             "expected at least one expression for the body to follow the contract"
             stx)]
           [(x . more)
            (keyword? (syntax-e #'x))
            (raise-syntax-error
             #f
             "unknown keyword"
             stx
             #'x)]
           [(body1 body2 ...)
            (values #'any #'(body1 body2 ...))]
           [()
            (raise-syntax-error
             #f
             "expected at least one expression for the body to follow the contract"
             stx)]))


       (with-syntax ([(internal-f) (generate-temporaries (list #'f))])
         #`(begin
             (define internal-f
               (syntax-parameterize ([current-contract-region (λ (stx) #`'(function f))])
                 (let ([f (λ (#,@arg-ids) #,@bodies)])
                   f)))
             #,(set-up-contract-boundary-via-identifiers
                #`(->i (#,@->i-doms) #,rng)
                #'internal-f      ;; identifier bound to the uncontracted value
                #'f               ;; identifier that names the value
                ;;                    (used in a let to name the wrapper)
                #'f               ;; identifier that gets the contract on it
                ;;                    (bound by the syntax in the result)
                'define-region
                #t
                #'f
                #`(quote-srcloc #,stx)
                #''(function f)
                #f)   ;; make-blame: positive blame party
       )))]))
