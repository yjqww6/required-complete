#lang racket/base
(require (for-syntax racket/base) racket/set)

(define-syntax (for/union stx)
  (syntax-case stx ()
    [(_ clauses body ... expr)
     (with-syntax ([orig stx])
       #'(for/fold/derived orig ([s (set)])
           clauses
           body ...
           (set-union s expr)))]))

(define (merge-imports imports)
  (for/union ([p (in-list imports)])
    (list->set (cdr p))))

(define (merge-exports exports)
  (for/union ([p (in-list exports)])
    (list->set (map car (cdr p)))))

(define (walk mod)
  (apply set-union (set mod)
         (map walk (append
                    (module-compiled-submodules mod #f)
                    (module-compiled-submodules mod #t)))))

(define (imported-modules fpe ev)
  (let ([cpl (compile fpe)])
    (ev fpe)
    (for/union ([mod (in-set (walk cpl))])
      (merge-imports (module-compiled-imports mod)))))

(define (imported-identifiers mods ev)
  (for/union ([mod (in-set mods)]
              #:when (ev #`(module-declared? #,mod)))
    (let-values ([(a b) (ev #`(module->exports #,mod))])
      (set-union (merge-exports a) (merge-exports b)))))

(define (ids fpe)
  (define ns (make-base-namespace))
  (define (ev v) (eval v ns))
  (define mods (imported-modules fpe ev))
  (imported-identifiers mods ev))

(provide ids)