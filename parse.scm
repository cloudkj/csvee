(use extras srfi-13 srfi-69)

(define (parse line columns)
  ;; helpers
  (define (select? col)
    (hash-table-ref/default columns col #f))

  ;; states
  (define (error msg)
    (print "ERROR: " msg)
    (list))

  (define (terminal from to col quoted? escaped? selected)
    (cond
     ;; quoted, but unclosed field
     ((and quoted? (not (eq? (string-ref line (- to 1)) #\")))
      (error "no closing quote"))
     ;; add field
     ((select? col)
      (cons (substring line from to) selected))
     (else selected)))

  (define (comma from to col quoted? escaped? selected)
    (cond
     ;; inside quoted field
     ((and quoted? (not (eq? (string-ref line (- to 1)) #\")))
      (next from (+ to 1) col quoted? escaped? selected))
     ;; end of selected field
     ((select? col)
      (next (+ to 1) (+ to 1) (+ col 1) #f #f (cons (substring line from to) selected)))
     ;; end of unselected field
     (else
      (next (+ to 1) (+ to 1) (+ col 1) #f #f selected))))

  (define (quote from to col quoted? escaped? selected)
    (cond
     ;; start of quoted field
     ((= from to)
      (next from (+ to 1) col #t escaped? selected))
     ;; not in a quoted field
     ((not quoted?)
      (error "not in a quoted field"))
     ;; initial, possibly unescaped quote
     ((not escaped?)
      (next from (+ to 1) col quoted? #t selected))
     ;; second, escaped quote
     ((eq? (string-ref line (- to 1)) #\")
      (next from (+ to 1) col quoted? #f selected))
     ;; unescaped, standalone quote
     (else (error "invalid quote"))))

  (define (any from to col quoted? escaped? selected)
    (next from (+ to 1) col quoted? escaped? selected))

  ;; starting state
  (define (next from to col quoted? escaped? selected)
    (let ((char (cond ((>= to (string-length line)) terminal)
                      ((eq? (string-ref line to) #\") quote)
                      ((eq? (string-ref line to) #\,) comma)
                      (else any))))
      (char from to col quoted? escaped? selected)))

  ;; TODO: do away with reverse
  (string-join (reverse (next 0 0 1 #f #f (list))) ","))
