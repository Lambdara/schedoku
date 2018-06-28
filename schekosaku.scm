;; A board is simply a list of cells, where every cell is a list of the possible
;; contents of that cell. In an empty board, every cell can still take every
;; possible value, so it's simply a list of 81 lists of 1 through 9

(define (range start end step)
  (if (> start  end)
      '()
      (cons start (range (+ start step) end step))))

(define (repeat item n)
    (if (= n 0)
        '()
        (cons item (repeat item (- n 1)))))

(define empty-board
  (repeat (list 1 2 3 4 5 6 7 8 9) 81))

(define (row-indices n)
  (range (* n 9) (- (* (+ n 1) 9) 1) 1))

(define (col-indices n)
  (range n 80 9))

(define (block-indices n)
    (apply append
           (map (lambda (x) (list x (+ x 9) (+ x 18)))
                (let* ((x (mod n 3))
                       (y (div n 3))
                       (m (+ (* 3 x) (* 27 y))))
                  (list m (+ m 1) (+ m 2))))))

(define (col-of x)
  (mod x 9))

(define (row-of x)
  (div x 9))

(define (block-of x)
  (+ (div (mod x 9) 3)
     (* 3 (div x 27))))

(define (map-indices board f indices)
  (define (go board f indices x)
    (if (null? indices)
        board
        (let ((in-indices? (= x (car indices))))
          (cons (if in-indices?
                    (f (car board))
                    (car board))
                (go (cdr board)
                    f
                    (if in-indices?
                        (cdr indices)
                        indices)
                    (+ x 1))))))
  (go board f (sort < indices) 0))

(define (union . lists)
  (define (binary-union l1 l2)
    (let ((sl1  (sort < l1))
          (sl2  (sort < l2))
          (union        '())
          (intersection '()))
      (let eat-lists
          ((fst sl1)
           (snd sl2))
        (if (not (and (eq? '() fst) (eq? '() snd)))
            (cond
             ((eq? '() fst)
              (set! union (append snd union)))
             ((eq? '() snd)
              (set! union (append fst union)))
             ((equal? (car fst) (car snd))
              (set! union (cons (car fst) union))
              (set! intersection (cons (car fst) intersection))
              (eat-lists (cdr fst) (cdr snd)))
             ((< (car fst) (car snd))
              (set! union (cons (car fst) union))
              (eat-lists (cdr fst) snd))
             ((> (car fst) (car snd))
              (set! union (cons (car snd) union))
              (eat-lists fst (cdr snd))))))
      union))
  (cond ((null? lists)
         '())
        ((null? (cdr lists))
         (car lists))
        (else
         (binary-union (car lists)
                       (apply union (cdr lists))))))

(define (print-sudoku board)
  (define (skip list n)
    (cond ((null? list) '())
          ((<= n 0) list)
          (else
           (skip (cdr list) (- n 1)))))
  (define (splice list n m)
    (let loop ((stop (- m n)) (list (skip list n)))
      (if (or (eq? stop 0) (null? list))
          '()
          (cons (car list) (loop (- stop 1) (cdr list))))))
  (define (group-for list count)
    (let ((step (/ (length list) count)))
      (let loop ((i 0))
        (if (eq? i count)
            '()
            (cons (splice list (* i count) (* (+ i 1) count)) 
                  (loop (+ i 1)))))))
  (define minimized
    (map (lambda (x)
           (cond ((null? x)
                  "!")
                 ((null? (cdr x))
                  (car x))
                 (else
                  "?")))
         board))
  (if (equal? board 'failed)
      (print "failed")
      (for-each (lambda (x) (display x) (newline)) (group-for minimized 9))))


(define (insert board item position)
  (define (indices-of position)
    (union (row-indices (row-of position))
           (col-indices (col-of position))
           (block-indices (block-of position))))
  ;; current part of board, (value,position) pairs of insertions to be done,
  ;; current value inserting, current position inserting at, position in board,
  ;; accumulator of result
  (define (go board jobs cur-val cur-pos indices index result)
    (cond
     ;; No jobs and at end of board -> done
     ((and (null? jobs) (null? board))
      (reverse result))
     ;; No board left -> next job
     ((null? board)
      (go (reverse result) (cdr jobs) (caar jobs) (cdar jobs) (indices-of (cdar jobs))  0 '()))
     (else
      (let ((old (car board))
            (new (cond ((= cur-pos index)
                        (list cur-val))
                       ((member index indices)
                        (filter (lambda (x) (not (= x cur-val))) (car board)))
                       (else (car board)))))
        (go (cdr board)
            (if (and (not (= index cur-pos))
                     (> (length old) 1)
                     (= (length new) 1))
                (cons (cons (car new) index) jobs)
                jobs)
            cur-val
            cur-pos
            indices
            (+ index 1)
            (cons new result))))))
  (go board
      '()
      item
      position
      (indices-of position)
      0
      '()))

;; Creates board from list of values
(define (make-board data)
  (define (go data board x)
    (cond ((null? data)
           board)
          ((= 0 (car data))
           (go (cdr data) board (+ x 1)))
          (else
	   (go (cdr data) (insert board (car data) x) (+ x 1)))))
  (go data empty-board 0))


(define (parse-sudoku input)
  (define (chars->nums cs)
    (map (lambda (x) (- x 48))
         (map char->integer cs)))
  (make-board (chars->nums (string->list input))))

(define test-inputs
  (list
   "700100000020000015000006390200018000040090070000750003078500000560000040000001002"
   "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
   "200080300060070084030500209000105408000000000402706000301007040720040060004010003"
   "000000907000420180000705026100904000050000040000507009920108000034059000507000000"
   "030050040008010500460000012070502080000603000040109030250000098001020600080060020"
   "020810740700003100090002805009040087400208003160030200302700060005600008076051090"
   "100920000524010000000000070050008102000000000402700090060000000000030945000071006"
   "043080250600000000000001094900004070000608000010200003820500000000000005034090710"
   "480006902002008001900370060840010200003704100001060049020085007700900600609200018"
   "000900002050123400030000160908000000070000090000000205091000050007439020400007000"
   "800000000003600000070090200050007000000045700000100030001000068008500010090000400"
   ))

(define (done board)
  (= 81 (length (filter (lambda (x) (= 1 (length x))) board))))


(define (search board)
  ;; Whether every entry has at least one option
  (define (error-free board)
    (= 0 (length (filter null? board))))
  ;; Tries all the values at the location and searches,
  ;; If failing, 'failed. Otherwise, result
  (define (try-inserts values location)
    (if (null? values)
	'failed
	(let ((new-board (insert board (car values) location)))
	  (if (error-free new-board)
	      (let ((outcome (search new-board)))
		(if (equal? 'failed outcome)
		    (try-inserts (cdr values) location)
		    outcome))
	      (try-inserts (cdr values) location)))))

  ;; Next choice on the board
  (define (choice board)
    (define (go board x)
      (if (> (length (car board)) 1)
	  (cons (car board) x)
	  (go (cdr board) (+ x 1))))
    (go board 0))

  (if (done board)
      board
      (let ((next-choice (choice board)))
	(try-inserts (car next-choice) (cdr next-choice)))))

;; Maps input of string to pretty-printed result
(define (solve input)
  (print-sudoku (search (parse-sudoku input))))
