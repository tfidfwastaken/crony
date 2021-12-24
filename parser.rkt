#lang racket/base

(require data/monad data/applicative
         racket/list racket/function
         megaparsack megaparsack/text
         "crony.rkt")

(define star/p (char/p #\*))
(define rangesep/p (char/p #\-))
(define listsep/p (char/p #\,))
(define stepsep/p (char/p #\/))

;; Shorthand to parse a value that *might* be there
(define (maybe/p parser)
  (many/p #:min 0 #:max 1 parser))

;; Parses a step interval, ie /5 or /3
(define step/p (do stepsep/p integer/p))

(define days '("sun" "mon" "tue" "wed" "thu" "fri" "sat"))

(define dayname/p (or/p (string-ci/p "sun")
                        (string-ci/p "mon")
                        (string-ci/p "tue")
                        (string-ci/p "wed")
                        (string-ci/p "thu")
                        (string-ci/p "fri")
                        (string-ci/p "sat")))

;; Parses either a day string as day index, or the day index
(define days/p (do [day <- dayname/p]
                   (pure (index-of days day))))

(define value/p
  (do (or/p integer/p
            days/p)))

;; Parses a field range, either specified via interval or a
;; hyphenated range.
(define (fieldrange/p end)
  (or/p (try/p
         (do [from <- value/p]
             stepsep/p
           [interval <- integer/p]
           (pure (range from end interval))))
        (try/p
         (do [from <- value/p]
             rangesep/p
           [to <- value/p]
           [interval <- (maybe/p step/p)]
           (pure (range from (add1 to) (if (empty? interval) 1
                                    (first interval))))))
        (do [val <- value/p]
            (pure (list val)))))

;; Parses a cron field for a given range and generates it using
;; the provided functions.
;;
;; Examples of cron fields:
;;  */5
;;  12-15,6,3-4/2
(define (field/p gen-star gen-list end)
  (or/p (do star/p
            [interval <- (maybe/p step/p)]
          (pure (gen-star interval)))
        (do [vals <- (many/p (fieldrange/p end)
                             #:sep listsep/p)]
            (pure (gen-list vals)))))

;; Creates a field parser for a given range.
(define (make-field-parser from to)
  (let ([gen-star (λ (interval)
                    (if (empty? interval)
                        '(#\*)
                        (range from to (first interval))))]
        [gen-list (compose (curry map (curryr remainder to))
                           remove-duplicates flatten)])
    (field/p gen-star gen-list to)))

(define minute-field/p (make-field-parser 0 60))
(define hours-field/p (make-field-parser 0 24))
(define day-of-month-field/p (make-field-parser 1 32))
(define month-field/p (make-field-parser 1 13))
(define day-of-week-field/p (make-field-parser 0 7))
(define any-string/p
  (do [chars <- (many+/p any-char/p)]
      (pure (list->string chars))))

;; Parses a cron spec.
(define cron/p
  (do [minutes <- minute-field/p]
      space/p
    [hours <- hours-field/p]
    space/p
    [day-of-month <- day-of-month-field/p]
    space/p
    [month <- month-field/p]
    space/p
    [day-of-week <- day-of-week-field/p]
    space/p
    [script <- any-string/p]
    (pure (crony minutes hours day-of-month month day-of-week script))))

(module+ test
  (require rackunit)
  (require data/either)

  (test-case
    "minute fields are parsed correctly"
    (check-equal?
     (parse-string minute-field/p "10-25/3")
     (success '(10 13 16 19 22 25)))
    (check-equal?
     (parse-string minute-field/p "55/1")
     (success '(55 56 57 58 59))))

  (test-case
    "hour fields are parsed correctly"
    (check-equal?
     (parse-string hours-field/p "12-25/3,3")
     (success '(12 15 18 21 0 3)))
    (check-equal?
     (parse-string hours-field/p "21/1")
     (success '(21 22 23))))

  (test-case
    "day-of-month fields are parsed correctly"
    (check-equal?
     (parse-string day-of-month-field/p "25/1,1")
     (success '(25 26 27 28 29 30 31 1)))
    (check-equal?
     (parse-string day-of-month-field/p "0-4,5-8")
     (success '(0 1 2 3 4 5 6 7 8))))

  (test-case
    "month fields are parsed correctly"
    (check-equal?
     (parse-string month-field/p "7-14")
     (success '(7 8 9 10 11 12 0 1)))
    (check-equal?
     (parse-string month-field/p "12/1")
     (success '(12))))

  (test-case
    "day-of-week fields are parsed correctly"
    (check-equal?
     (parse-string day-of-week-field/p "mon/1")
     (success '(1 2 3 4 5 6)))))
