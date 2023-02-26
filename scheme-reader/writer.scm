(import (scheme base)
        (only (scheme list) remove)
        (scheme char)
        (scheme write))

(load "./core.scm")

(define (%write obj output-port)
  (cond
    ((list? obj)
     (display "(")
     (for-each (lambda (o) (%write o output-port) (display " " output-port))
               obj)
     (display ")"))
    ((and (lexical? obj)
          (or (eq? (ref-type obj) 'SPACE)
              (eq? (ref-type obj) 'NEWLINE))))
    ((and (lexical? obj)
          (or (eq? (ref-type obj) 'COMMENT)))
     (display ";" output-port)
     (display (ref-data obj) output-port)
     (newline output-port))
    ((and (lexical? obj) (eq? (ref-type obj) 'ATOM))
     (display (ref-origin obj)))
    (else (write obj output-port))))


(define (write2 obj output-port)
  (%write obj output-port))
