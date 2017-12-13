(use extras)
(use getopt-long)

(define (parse line columns)
  ;; TODO: optimizations - `columns` can be looked up in O(1) and collected as
  ;; each index is examined
  (define (select from to count selected)
    (cond ((>= to (string-length line))
           (if (member (+ count 1) columns)
               (cons (substring line from to) selected)
               selected))
          ((eq? (string-ref line to) #\,)
           (select (+ to 1) (+ to 1) (+ count 1)
                     (if (member (+ count 1) columns)
                         (cons (substring line from to) selected)
                         selected)))
          (else
           (select from (+ to 1) count selected))))
  ;; TODO: select can simply print out selected columns
  (select 0 0 0 (list)))

(define (read-input columns)
  (let ((line (read-line)))
    (when (not (eof-object? line))
          (begin
            (print "parsed: " (parse line columns) "\n")
            (read-input columns)))))

(let* ((grammar '((column "Columns to select"
                          (single-char #\c)
                          (value #t))))
       (options (getopt-long (command-line-arguments) grammar))
       (columns (map (compose string->number cdr)
                     (filter (lambda (o) (equal? (car o) 'column)) options))))
  (cond ((null? columns) (print (usage grammar)))
        (else
         (read-input columns))))
