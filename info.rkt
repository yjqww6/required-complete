#lang info
(define collection "required-complete")
(define deps '("base" "drracket" "drracket-plugin-lib" "gui-lib" "srfi-lib"))
(define build-deps '())
(define pkg-desc "auto complete for required identifiers")
(define version "0.1")
(define pkg-authors '(yjqww6))


(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names '("required-complete"))
(define drracket-tool-icons '(#f))
