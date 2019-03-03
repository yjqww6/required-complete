#lang racket

(module backend racket
  (require syntax/modresolve scribble/xref setup/xref
           scribble/manual-struct scribble/blueboxes)

  (define xref 
    (load-collections-xref))

  (define entrys (xref-index xref))

  (define cache (make-blueboxes-cache #t))

  (define h
    (let ([h (make-hash)])
      (for ([en (in-list entrys)])
        (match en
          [(entry _ _ _ (method-index-desc name from-libs mn t))
           (for ([lib (in-list from-libs)])
             (hash-update! h (resolve-module-path lib)
                           (λ (s) (set-add! s mn) s)
                           (λ () (mutable-set))))]
          [(entry _ _ _ (constructor-index-desc name from-libs t))
           (define strs (fetch-blueboxes-strs t #:blueboxes-cache cache))
           (for ([str (in-list strs)])
             (define ls
               (regexp-match* #px"\\[([^\\[\\s ]+).*?\\]" str
                              #:match-select cadr))
             (for ([lib (in-list from-libs)])
               (hash-update!
                h (resolve-module-path lib)
                (λ (s) (set-union! s (list->mutable-set (map string->symbol ls))) s)
                (λ () (mutable-set)))))]
          [else (void)]))
      h))
  (define datum
    (for/hash ([(k v) (in-hash h)])
      (values k (set->list v))))
  (provide datum))

(require racket/place syntax/location)

(define mod (quote-module-path backend))

(define datum (hash))

(define (entry)
  (sync (system-idle-evt))
  (define p
    (place
     ch
     (define d (dynamic-require mod 'datum))
     (place-channel-put ch d)))
  (set! datum (for/hash ([(k v) (in-hash (place-channel-get p))])
                (values k (list->set v)))))

(define (get-datum r) (hash-ref datum r (λ () (set))))

(void (thread entry))

(provide get-datum)