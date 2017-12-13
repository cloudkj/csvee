(use extras)
(use getopt-long)

(define (parse line columns)
  ;; TODO: optimizations - `columns` can be looked up in O(1) and collected as
  ;; each index is examined
  (define (output from to delim)
    (format #t (if delim ",~A" "~A") (substring line from to)))
  (define (select from to count many?)
    (cond ((>= to (string-length line))
           (begin
             (when (member (+ count 1) columns)
                   (output from to many?))
             (print)))
          ((eq? (string-ref line to) #\,)
           (begin
             (when (member (+ count 1) columns)
                   (output from to many?))
             (select (+ to 1) (+ to 1) (+ count 1) #t)))
          (else
           (select from (+ to 1) count many?))))
  (select 0 0 0 #f))

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
       (columns (map (compose string->number cdr)
                     (filter (lambda (o) (equal? (car o) 'column)) options))))
  (cond ((null? columns) (print (usage grammar)))
        (else
         (read-input columns))))
