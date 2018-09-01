(load "patterns.ss")

(define patterns-by-line
  (fold-right 
    (lambda (char acc)
      (if (equal? char #\newline)
        (cons '() acc)
        (if (not (null? acc))
          (cons (cons char (car acc)) (cdr acc))
          (cons char acc))))
    '() (string->list patterns)))

(define (is-colored? el)
  (equal? el #\#))

(define (color index row)
  (define (number-of-colored-neighbours row index)
    (define (number-of-colored-in-range range)
      (fold-right (lambda (x acc) (if (is-colored? x) (+ acc 1) acc))
                  0 range))
    (let* ([start (max (- index 2) 0)]
           [end (min (+ index 2) (- (length row) 1))]
           [left (list-tail (list-head row index) start)]
           [right (list-tail (list-head row end) index)])
      (+ (number-of-colored-in-range left)
         (number-of-colored-in-range right))))
  (let ([colored (is-colored? (list-ref row index))]
        [colored-neighbours (number-of-colored-neighbours row index)])
    (cond 
      [(and colored (or (= colored-neighbours 2) (= colored-neighbours 4))) #\#]
      [(and (not colored) (or (= colored-neighbours 2) (= colored-neighbours 3))) #\#]
      [else #\.])))

(define (generate-next-row row)
  (define (generate-next-row-recursive index row new-row)
    (cond
      [(< index (length row))
       (generate-next-row-recursive (+ index 1) row (cons (color index row) new-row))]
      [else (reverse new-row)]))
  (generate-next-row-recursive
    0
    row
    '()))

(define (type latest-row)
  (define (type-based-on-latest-row latest-row rows)
    (define (vanishing? latest-row rows)
      (not (exists is-colored? latest-row)))
    (define (blinking? latest-row rows)
      (exists (lambda (row) (equal? latest-row row)) rows))
    (define (gliding? latest-row rows)
      #f)
    (cond
      [(vanishing? latest-row rows) "vanishing"]
      [(blinking? latest-row rows) "blinking"]
      [(gliding? latest-row rows) "gliding"]
      [else "other"]))
  (define (type-recursive latest-row rows res)
    (cond 
      [(not (equal? res "other")) res]
      [(<= (length rows) 100) 
       (type-recursive (generate-next-row latest-row) (cons latest-row rows) (type-based-on-latest-row latest-row rows))]
      [else res]))
  (type-recursive (generate-next-row latest-row) (list latest-row) "other"))

(for-each 
  (lambda (x) (display (type x)) (newline))
  patterns-by-line)

