(define input "##.######
  #.###......................#.###......................####......................###.#......................###.#
  #######
  #.#..#...####..##..##..##
  ###.#....#.###
  ########
  ##...#.###########
  #.#..#...####..##..##..##.....##
  #######.##.##.#.#....#.######
  #.######
  ##....#.#....#.....#....#....#.....###.#
  #.###........................................................#######........................................................###.#
  #...###...#.#
  #...#.#..###...#
  #########
  #######.##.##.#.#
  #...#...#...#...#...#...#...#...#...#...#
  #..##.#..#
  #.###...................................................###.#
  ######
  #...#...#...#...#...#...#...#...#...#...#....#######.##.##.#.#
  ")

(define (create-colored)
  'c)
(define (create-uncolored)
  'u)
(define (create-newline)
  'n)

(define (sanitize-input input)
  (define (symbolize-string input-string)
    (define (symbolize char)
      (cond 
        [(equal? char #\#) (create-colored)]
        [(equal? char #\.) (create-uncolored)]
        [(equal? char #\newline) (create-newline)]))
    (define (symbolizable? char)
      (or (equal? char #\#) (equal? char #\.) (equal? char #\newline)))
    (fold-right (lambda (char acc) (if (symbolizable? char)
                                     (cons (symbolize char) acc)
                                     acc))
                '()
                (string->list input-string)
                ))
  (fold-right 
    (lambda (symbol acc)
      (if (equal? symbol (create-newline))
        (cons '() acc)
        (if (not (null? acc))
          (cons (cons symbol (car acc)) (cdr acc))
          (cons symbol acc))))
    '() 
    (symbolize-string input)))

(define (is-colored? el)
  (equal? el (create-colored)))

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
      [(and colored (or (= colored-neighbours 2) (= colored-neighbours 4))) (create-colored)]
      [(and (not colored) (or (= colored-neighbours 2) (= colored-neighbours 3))) (create-colored)]
      [else (create-uncolored)])))

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

(define (recognize-pattern starting-pattern)
  (define (recognize-pattern-recursive latest-row rows res)
    (define (recognize-pattern-from-latest-row latest-row rows)
      (define (vanishing? latest-row rows)
        (not (exists is-colored? latest-row)))
      (define (blinking? latest-row rows)
        (exists (lambda (row) (equal? latest-row row)) rows))
      (define (gliding? latest-row rows)
        (define (can-be-shifted-right? row)
          (not (is-colored? (car (reverse row)))))
        (define (shift-right row)
          (cons (create-uncolored) (reverse (cdr (reverse row)))))
        (or (and (can-be-shifted-right? latest-row)
                 (exists (lambda (row) (equal? (shift-right latest-row) row)) rows))
            (and (can-be-shifted-right? (reverse latest-row))
                 (exists (lambda (row) (equal? (reverse (shift-right (reverse latest-row))) row)) rows))))
      (cond
        [(vanishing? latest-row rows) "vanishing"]
        [(blinking? latest-row rows) "blinking"]
        [(gliding? latest-row rows) "gliding"]
        [else "other"]))
    (cond 
      [(not (equal? res "other")) res]
      [(<= (length rows) 100) 
       (recognize-pattern-recursive 
         (generate-next-row latest-row) 
         (cons latest-row rows) 
         (recognize-pattern-from-latest-row latest-row rows))]
      [else res]))
  (recognize-pattern-recursive (generate-next-row starting-pattern) (list starting-pattern) "other"))

(for-each 
  (lambda (starting-pattern) (display (recognize-pattern starting-pattern)) (newline))
  (sanitize-input input))
