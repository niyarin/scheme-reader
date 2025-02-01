(import (scheme base)
        ;(scheme write)
        (srfi 78)
        (prefix (scheme-reader core) rdr/))


;;literal test
(check (rdr/read (open-input-string "some-symbol"))
       => 'some-symbol)

(check (rdr/read (open-input-string "some-symbol?"))
       => 'some-symbol?)

(check (rdr/read (open-input-string "#\\a"))
       => #\a)

(check (rdr/read (open-input-string "#\\newline"))
       => #\newline)

;; list test

(check (rdr/read (open-input-string "(foo bar baz)"))
       => '(foo bar baz))


(check (rdr/read (open-input-string "((foo bar) baz)"))
       => '((foo bar) baz))

;; delimiter test

(check (rdr/read (open-input-string "#\\newline list"))
       => #\newline)

(check (rdr/read (open-input-string "#\\newline\nlist"))
       => #\newline)


;; double read test

(let ((port (open-input-string "#\\newline\nlist")))
  (check (rdr/read port) => #\newline)
  (check (rdr/read port) => 'list))

(check-report)

