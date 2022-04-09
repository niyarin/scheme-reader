(define-library (scheme-reader core)
  (import (scheme base)
          (scheme case-lambda)
          (only (scheme read) read-char))
  (export read)
  (begin
    (define-record-type <read-state>
      (%%make-read-state line colmn)
      %read-state?
      (line %read-state-line %set-read-state-line!)
      (column %read-state-column %set-read-state-column!))

    (define (%make-read-state) (%%make-read-state 0 0))

    (define (%inc-col! state)
      (%set-read-state-column! state (+ (%read-state-column state) 1)))

    (define (%read-delimiters state port)
      (let ((pkc (peek-char port)))
        (cond
          ((char-whitespace? pkc)
           (begin (read-char port)
                  (%inc-col! pkc)
                  pkc))
      )

    (define (%read state port)
      (%read-delimiters state port))

    (define %current-state-entity (make-parameter #f))

    (define (current-state) (%current-state-entity))

    (define read
      (case-lambda
        (() (read (current-input-port)))
        ((port) (%read (current-state) port))))))
