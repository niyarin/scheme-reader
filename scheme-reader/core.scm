(import (scheme base)
        (only (scheme list) filter)
        (scheme char)
        (scheme write))
;;まだドット対は未サポート

(define-record-type <lexical>
  (make-lexical type data)
  lexical?
  (type ref-type)
  (data ref-data))


(define (read-list port)
  (read-char port)
  (let loop ((ls '()))
    (let ((pc (peek-char port)))
      (cond
        ((char=? pc #\)) ;;end
         (reverse ls))
        ((eof-object? pc) (error "READ END!"))
        (else (loop (cons (get-token port) ls)))))))

(define (read-online-comment port)
  (read-char port)
  (let loop ((ls '()))
    (let ((c (read-char port)))
      (if (or (char=? c #\newline)
              (char=? c #\return))
        (make-lexical 'COMMENT (list->string (reverse ls)))
        (loop (cons c ls))))))

(define (char-special-initial? c)
  (case c
    ((#\! #\$ #\% #\& #\* #\\ #\: #\< #\= #\> #\? #\^ #\_ #\~) #t)
    (else #f)))

(define (read-symbol port)
  ;;<initial> <subsequent>*
  (let loop ((ls '()))
    (let ((pc (peek-char port)))
      (if (or (char-special-initial? pc)
               (char-alphabetic? pc)
               (char-numeric? pc))
        (loop (cons (read-char port) ls))
        (string->symbol (list->string (reverse ls)))))))

(define (get-token port)
  (let ((pc (peek-char port)))
    (cond
      ((eq? pc #\()
       (read-list port))
      ((or (char-alphabetic? pc)
           (char-special-initial? pc))
        (read-symbol port))
      ((or (eq? pc #\space)
           (eq? pc #\tab))
       (make-lexical 'SPACE (read-char port)))
      ((or (eq? pc #\newline)
           (eq? pc #\return))
       (make-lexical 'NEWLINE (read-char port)))
      ((eq? pc #\#)
        ;;sharp-read
       )
      ((eq? pc #\;)
       ;;read-online-comment
       (read-online-comment port))
      ((eq? pc #\|)
       )
      ((eq? pc #\"))
      (else
        (read-char port)))))

(define (read-internal port)
  (let loop ((res '()))
    (let ((tkn (get-token port)))
      (if (and (lexical? tkn)
               (or (eq? (ref-type tkn) 'SPACE)
                   (eq? (ref-type tkn) 'NEWLINE)
                   (eq? (ref-type tkn) 'COMMENT)))
        (loop (cons tkn res))
        (reverse (cons tkn res))))))

(define (visual-literal? obj)
  (lexical? obj))

(define (remove-visual-literals obj)
  (cond
    ((list? obj)
     (map remove-visual-literals (filter (lambda (x) (not (visual-literal? x))) obj)))
    (else obj)))

(define (read . args)
  (let ((port (or (and (not (null? args)) (car args))
                  (current-input-port))))
    (let ((res (remove-visual-literals (read-internal port))))
      (if (null? res)
        (error "ERROR")
        (car res)))))
