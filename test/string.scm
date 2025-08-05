(import (scheme base)
        (scheme write)
        (srfi 78)
        (prefix (scheme-reader core) rdr/))


(let ((input "\"hello world\""))
  (check (rdr/lexical-origin (car (rdr/read-internal (open-input-string input))))
         => input))


(let ((input  "\"hello \\\nworld\""))
  (check (rdr/lexical-origin (car (rdr/read-internal (open-input-string input))))
         => input))

(let ((input  "\"hello \nworld\""))
  (check (rdr/lexical-origin (car (rdr/read-internal (open-input-string input))))
         => input))

(let ((input  "\"h\\x65;llo-world\""))
  (check (rdr/lexical-origin (car (rdr/read-internal (open-input-string input))))
         => input))

(check-report)
