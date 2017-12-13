(use extras)
(use getopt-long)

(define (parse line columns)
  (define (select? col)
    (hash-table-ref/default columns col #f))
  (define (output from to delim)
    (format #t (if delim ",~A" "~A") (substring line from to)))
  (define (select from to col selected)
    (cond ((>= to (string-length line))
           (begin
             (when (select? col) (output from to (> selected 0)))
             (print)))
          ((eq? (string-ref line to) #\,)
           (begin
             (when (select? col) (output from to (> selected 0)))
             (select (+ to 1) (+ to 1) (+ col 1)
                     (if (select? col) (+ selected 1) selected))))
          (else
           (select from (+ to 1) col selected))))
  (select 0 0 1 0))

(define (read-input columns)
  (let ((line (read-line)))
    (when (not (eof-object? line))
          (begin
            (parse line columns)
            (read-input columns)))))

(let* ((grammar '((column "Columns to select"
                          (single-char #\c)
                          (value #t))))
       (options (getopt-long (command-line-arguments) grammar))
       (columns (alist->hash-table
                 (map (lambda (n) (cons n #t))
                      (map (compose string->number cdr)
                           (filter (lambda (o) (equal? (car o) 'column)) options))))))
  (if (null? columns)
      (print (usage grammar))
      (read-input columns)))
