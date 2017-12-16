(use srfi-69 test)

(load "parse.scm")

(define (columns . cols)
  (alist->hash-table (map (lambda (x) (cons x #t)) cols)))

(test "foo,bar,baz"     (parse "foo,bar,baz" (columns 1 2 3)))
(test "foo,baz"         (parse "foo,bar,baz" (columns 1 3)))
(test "bar"             (parse "foo,bar,baz" (columns 2)))
(test "\"bar\""         (parse "foo,\"bar\",baz" (columns 2)))
(test "\"bark\"\"ley\"" (parse "foo,\"bark\"\"ley\",baz" (columns 2)))
(test "\"foo,bar,baz\"" (parse "\"foo,bar,baz\"" (columns 1)))

(test "" (parse "foo,\"bark\"ley\",baz" (columns 2)))
(test "" (parse "foo,bark\"ley\",baz"   (columns 2)))
(test "" (parse "foo,\"bark\"  \"ley\",baz"   (columns 2)))
(test "" (parse "foo,\"bark\"\"ley,baz"   (columns 2)))
(test "" (parse "foo,\"bark\"\"le\"y,baz"   (columns 2)))
