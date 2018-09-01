(load "patterns.ss")

(define starting-rows
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

(define (color-below row index)
  (define (number-of-colored-neighbours row index colored)
    (define (number-of-colored-in-range range)
      (fold-right (lambda (x acc) (if (is-colored? x) (+ acc 1) acc)) 0 range))
    (let* ([start (max (- index 2) 0)]
           [end (min (+ index 2) (- (length row) 1))]
           [range (list-tail (list-head row (+ end 1)) start)])
      (if colored (- (number-of-colored-in-range range) 1)
        (number-of-colored-in-range range))))
  (let* ([colored (is-colored? (list-ref row index))]
         [colored-neighbours (number-of-colored-neighbours row index colored)])
    (cond 
      [(and colored (or (= colored-neighbours 2) (= colored-neighbours 4))) #\#]
      [(and (not colored) (or (= colored-neighbours 2) (= colored-neighbours 3))) #\#]
      [else #\.])))

(define (generate-next-row row)
  (define (generate-next-row-recursive row index new-row)
    (cond
      [(< index (length row))
       (generate-next-row-recursive row (+ index 1) (cons (color-below row index) new-row))]
      [else (reverse new-row)]))
  (generate-next-row-recursive
    row
    0
    '()))

(define (recognize-pattern starting-row)
  (define (recognize-pattern-from-latest-row latest-row rows)
    (define (vanishing? latest-row rows)
      (not (exists is-colored? latest-row)))
    (define (blinking? latest-row rows)
      (exists (lambda (row) (equal? latest-row row)) rows))
    (define (gliding? latest-row rows)
      (define (can-be-shifted-right? row)
        (not (is-colored? (car (reverse row)))))
      (define (shift-right row)
        (cons #\. (reverse (cdr (reverse row)))))
      (or (and (can-be-shifted-right? latest-row)
               (exists (lambda (row) (equal? (shift-right latest-row) row)) rows))
          (and (can-be-shifted-right? (reverse latest-row))
               (exists (lambda (row) (equal? (reverse (shift-right (reverse latest-row))) row)) rows))))
    (cond
      [(vanishing? latest-row rows) "vanishing"]
      [(blinking? latest-row rows) "blinking"]
      [(gliding? latest-row rows) "gliding"]
      [else "other"]))
  (define (recognize-pattern-recursive latest-row rows res)
    (cond 
      [(not (equal? res "other")) res]
      [(<= (length rows) 100) 
       (recognize-pattern-recursive 
         (generate-next-row latest-row) 
         (cons latest-row rows) 
         (recognize-pattern-from-latest-row latest-row rows))]
      [else res]))
  (recognize-pattern-recursive (generate-next-row starting-row) (list starting-row) "other"))

(for-each 
  (lambda (starting-row) (display (recognize-pattern starting-row)) (newline))
  starting-rows)

