(import (scheme base)
        ;(scheme write)
        (srfi 78)
        (scheme char)
        (prefix (scheme-reader core) rdr/))


;;literal test
(check (rdr/read (open-input-string "some-symbol"))
       => 'some-symbol)

(check (rdr/read (open-input-string "some/symbol"))
       => 'some/symbol)

(check (rdr/read (open-input-string "some-symbol?"))
       => 'some-symbol?)

(check (rdr/read (open-input-string "+"))
       => '+)

(check (rdr/read (open-input-string "a-b"))
       => 'a-b)

(check (rdr/read (open-input-string "-3"))
       => -3)


(check (rdr/read (open-input-string "1.234"))
       => 1.234)

(check (rdr/read (open-input-string "#\\a"))
       => #\a)

(check (rdr/read (open-input-string "#\\!"))
       => #\!)

(check (rdr/read (open-input-string "#\\newline"))
       => #\newline)

(check (rdr/read (open-input-string "|hello|"))
       => 'hello)

(check (rdr/read (open-input-string "|h\x65;llo|"))
       => 'hello)

(check (rdr/read (open-input-string "|he\\|\\|o|"))
       => (string->symbol "he||o"))

(check (rdr/read (open-input-string "#t"))
       => #t)

(check (rdr/read (open-input-string "#true"))
       => #t)

(check (rdr/read (open-input-string "#f"))
       => #f)

(check (rdr/read (open-input-string "#false"))
       => #f)

(check (rdr/read (open-input-string "..."))
       => '...)


(check (rdr/read (open-input-string "#x00A0"))
       => 160)

;; list test

(check (rdr/read (open-input-string "(foo bar baz)"))
       => '(foo bar baz))


(check (rdr/read (open-input-string "(foo bar baz   )"))
       => '(foo bar baz))


(check (rdr/read (open-input-string "(foo bar . baz)"))
       => '(foo bar . baz))

(check (rdr/read (open-input-string "(foo bar . (a b c) )"))
       => '(foo bar . (a b c)))

(check (rdr/read (open-input-string "((foo bar) baz)"))
       => '((foo bar) baz))

(check (rdr/read (open-input-string "\"abc\\x64;e\""))
       => "abcde")

;; delimiter test

(check (rdr/read (open-input-string "#\\newline list"))
       => #\newline)

(check (rdr/read (open-input-string "#\\newline\nlist"))
       => #\newline)


;; multi line string test
(check (rdr/read (open-input-string "\"abc \\     \ndef\""))
       => "abc def")

(check (rdr/read (open-input-string "\"abc \\\ndef\""))
       => "abc def")

(check (rdr/read (open-input-string "\"abc \\\r\ndef\""))
       => "abc def")

;; double read test

(let ((port (open-input-string "#\\newline\nlist")))
  (check (rdr/read port) => #\newline)
  (check (rdr/read port) => 'list))

;; check shebang

(check (rdr/lexical? (rdr/read-internal-or-handle-shebang
                       (open-input-string "#!/usr/bin/gosh\n")))
       => #t)
(check (rdr/read-internal-or-handle-shebang
         (open-input-string "#\\a"))
       => #\a)

;; check #!fold-case
(check (rdr/read (open-input-string "#!fold-case HELLO"))
       => (string->symbol (string-foldcase "HELLO")))

(check (rdr/read (open-input-string "#!no-fold-case HELLO"))
       => 'HELLO)

;; expand
(check (rdr/lexical-type
         (rdr/read (open-input-string "#:version")))
       => 'KEYWORD)

(check-report)

