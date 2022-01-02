#lang racket/base

(require txexpr
         "crony.rkt")

(define (make-cron-plist parsed-crony label)
  `(dict (assoc-pair "Label" ,label)
         (assoc-pair "RunAtLoad" (true))
         (assoc-pair "ProgramArguments"
                     (array ,@(for/list ([arg (crony-script parsed-crony)])
                                (list 'string arg))))))
