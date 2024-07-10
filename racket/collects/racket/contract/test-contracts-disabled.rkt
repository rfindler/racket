#lang racket/base

(define-syntax-rule
  (try expr)
  (try/proc 'expr (λ () expr)))
(define (try/proc name thunk)
  (with-handlers ([exn:fail?
                   (λ (exn)
                     (printf "#### ~s wrong: ~a\n"
                             name
                             (car (regexp-split #rx"\n" (exn-message exn)))))])
    (thunk)
    (printf "~s passed\n" name)
    (void)))

(module server1 racket/base
  (require racket/contract)
  (provide (contract-out [f1 (-> integer? integer?)]))
  (define (f1 x) x))
(require 'server1)
(try (f1 #f))

(module server2 typed/racket/base
  (provide f2)
  (: f2 (-> Integer Integer))
  (define (f2 x) x))
(require 'server2)
(try (f2 #f))

(require racket/contract)
(define/contract (f3 x)
  (-> integer? integer?)
  x)
(try (f3 #f))

(define f4 (contract (-> integer? integer?) values 'pos 'neg))
(try (f4 #f))

(module server5 racket/base
  (require racket/contract)
  (provide (contract-out [f5 (values (-> integer? integer?))]))
  (define (f5 x) x))
(require 'server5)
(try (f5 #f))