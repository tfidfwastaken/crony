#lang racket/base

(provide (struct-out crony))

;; all elements are of type:
;; (U (Listof Integer) 'all)
(struct crony
  (minutes hours monthdays weekdays months script))
