#lang racket
(require (for-syntax racket/base) syntax/kerncase)
(provide shrink-module)
(define (shrink-module fpe)
  (let ([fpe (syntax-disarm fpe #f)])

    (define raw-specs '())

    (define (push! sth)
      (when (ext-module-path? sth)
        (set! raw-specs (cons sth raw-specs))))

    (define (ext-module-path? r)
      (syntax-case* r (submod quote) (λ (a b) (free-identifier=? a b #f #f))
        [(submod "." ?id ...) #f]
        [(submod ".." ?id ...) #f]
        [(submod ?id) #f]
        [(quote ?id) #f]
        [_ #t]))

    (define (phaseless-spec spec)
      (syntax-case* spec
        (only prefix all-except prefix-all-except rename)
        (λ (a b) (free-identifier=? a b #f #f))
        [(only ?raw-module-path ?id ...)
         (push! #'?raw-module-path)]
        [(prefix ?prefix-id ?raw-module-path)
         (push! #'?raw-module-path)]
        [(all-except ?raw-module-path ?id ...)
         (push! #'?raw-module-path)]
        [(prefix-all-except ?prefix-id ?raw-module-path ?id ...)
         (push! #'?raw-module-path)]
        [(rename ?raw-module-path _ _)
         (push! #'?raw-module-path)]
        [?raw-module-path
         (push! #'?raw-module-path)]))

    (define (phaseless-spec* spec*)
      (for ([spec (in-syntax spec*)])
        (phaseless-spec spec)))
    
    (define (raw-require-spec spec)
      (syntax-case* spec
        (for-meta for-syntax for-template for-label just-meta)
        (λ (a b) (free-identifier=? a b #f #f))
        [(for-meta ?level ?phaseless-spec ...)
         (phaseless-spec* #'(?phaseless-spec ...))]
        [(for-syntax ?phaseless-spec ...)
         (phaseless-spec* #'(?phaseless-spec ...))]
        [(for-template ?phaseless-spec ...)
         (phaseless-spec* #'(?phaseless-spec ...))]
        [(for-label ?phaseless-spec ...)
         (phaseless-spec* #'(?phaseless-spec ...))]
        [(just-meta ?level ?raw-require-spec ...)
         (for ([spec (in-syntax #'(?raw-require-spec ...))])
           (raw-require-spec spec))]
        [?phaseless-spec
         (phaseless-spec #'?phaseless-spec)]))

    (define (walk form)
      (syntax-case* form (module module* #%require begin begin-for-syntax)
        (λ (a b) (free-identifier=? a b #f #f))
        [(module ?id ?path (#%plain-module-begin ?form ...))
         (push! #'?path)
         (walk* #'(?form ...))]
        [(module* ?id ?path (#%plain-module-begin ?form ...))
         (when (syntax-e #'?path)
           (push! #'?path))
         (walk* #'(?form ...))]
        [(#%require ?spec ...)
         (for ([spec (in-syntax #'(?spec ...))])
           (raw-require-spec spec))]
        [(begin ?form ...)
         (walk* #'(?form ...))]
        [(begin-for-syntax ?form ...)
         (walk* #'(?form ...))]
        [_ (void)]))

    (define (walk* form*)
      (for-each walk (syntax->list form*)))
    
    (syntax-rearm
     (kernel-syntax-case fpe #f
       [(module ?id ?path (#%plain-module-begin ?form ...))
        (begin
          (walk* #'(?form ...))
          (with-syntax ([(raw-specs ...) raw-specs])
            #'(module ?id ?path (#%plain-module-begin (#%require (only raw-specs) ...)))))])
     fpe)))