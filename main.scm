(declare (uses parse))

(use getopt-long)

(define (read-input columns)
  (let ((line (read-line)))
    (when (not (eof-object? line))
          (begin
            (print (parse line columns))
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
