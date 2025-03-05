(define-library (scheme-reader core)
  (import (scheme base)
          (only (scheme list) remove)
          (scheme char)
          (scheme write))
  (export read read-internal lexical? lexical-type lexical-data lexical-origin)
  ;;まだドット対は未サポート
  (begin
    (define-record-type <lexical>
      (%make-lexical type data origin)
      lexical?
      (type ref-type)
      (data ref-data)
      (origin ref-origin))

    (define lexical-type ref-type)
    (define lexical-data ref-data)
    (define lexical-origin ref-origin)

    (define (make-lexical type data)
      (%make-lexical type data #f))

    (define (make-lexical-dot-pair rests-list)
      (%make-lexical 'DOT-PAIR rests-list #f))

    (define (read-pair port)
      (read-char port)
      (let ((res-pair-head (list '())))
        (let loop ((res-pair res-pair-head))
          (let ((pc (peek-char port)))
            (cond
              ((char=? pc #\)) (read-char port) (cdr res-pair-head))
              ((char=? pc #\.)
               (read-char port)
               (let ((rests (list (get-token port))))
                 (set-cdr! res-pair (make-lexical-dot-pair rests))
                 (loop rests)))
              ((eof-object? pc) (error "READ END!"))
              (else
                (set-cdr! res-pair (list '()))
                (set-car! (cdr res-pair) (get-token port))
                (loop (cdr res-pair))))))))

    (define (read-oneline-comment port)
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

    (define (char-explicit-sign? c)
      (or (char=? c #\+) (char=? c #\-)))

    (define (char-special-subsequent? c)
      (or (char-explicit-sign? c)
          (char=? c #\.)
          (char=? c #\@)))

    (define (read-identifier port)
      ;;<initial> <subsequent>*
      ;; NOTE: This procedure does not check whether the first character is <initial>.
      (let loop ((ls '()))
        (let ((pc (peek-char port)))
          (if (and (not (eof-object? pc))
                   (or (char-special-initial? pc)
                       (char-special-subsequent? pc)
                       (char-alphabetic? pc)
                       (char-numeric? pc)))
            (loop (cons (read-char port) ls))
            (string->symbol (list->string (reverse ls)))))))

    (define (read-hex-scalr-value port)
      ;; <hex digit> +
      (let loop ((res 0))
        (let* ((pc (peek-char port))
               (head-char (or (and (char-numeric? pc) #\0)
                              (and (char<=? #\a pc #\f) #\a))))
              (if head-char
                (loop (+ (* res 16)
                         (- (char->integer (read-char port))
                            (char->integer head-char))))
                res))))

    (define (read-vertical-var-identifier port)
        ;; <vertical line> <symbol element>* <vertical line>
        (read-char port);read vertical line
        (let loop ((ls '()))
          (let ((pc (peek-char port)))
            (cond
              ((eof-object? pc) (error "Lexical error: not closed vertical line."))
              ((char=? pc #\|);;close
               (read-char port)
               (string->symbol (list->string (reverse ls))))
              ((not (char=? pc #\\))
               (loop (cons (read-char port) ls)))
              ((and (char=? (peek-char port) #\x)
                    (read-char port)
                    (read-hex-scalr-value port))
               ;;inline hex escape
               => (lambda (v)
                    (read-char port);; read #\;
                    (loop (cons (integer->char v) ls))))
              ((case (and (read-char port) (peek-char port))
                     ((#\a #\b #\t #\n #\r #\|) #t) (else #f))
               (loop (cons (read-char port) ls)))
              (else (error "Lexical error: invalid char." (peek-char port)))))))

    (define (%read-char-literal port)
      (read-char port)
      (let loop ((ls (list (read-char port))))
        (let ((pc (peek-char port)))
          (cond
            ((and (not (eof-object? pc))
                  (char-lower-case? pc)) (loop (cons (read-char port) ls)))
            ((null? (cdr ls)) (car ls))
            ((list->string (reverse ls))
             => (lambda (s)
                  (cond
                    ((string=? s "newline") #\newline)
                    ((string=? s "null") #\null)
                    ((string=? s "return") #\return)
                    ((string=? s "space") #\space)
                    ((string=? s "tab") #\tab)
                    ((string=? s "escape") #\escape)
                    ((string=? s "alarm") #\alarm)
                    ((string=? s "backspace") #\backspace)
                    (else (error "Invalid literal." s)))))))))

    (define (%read-true-literal port)
      (read-char port)
      (let ((pc (peek-char port)))
        (if (char=? pc #\r)
          (begin
            (read-char port)
            (if (and (char=? (read-char port) #\u) (char=? (read-char port) #\e))
              (%make-lexical 'ATOM #t "#true")
              (error "Invalid literal.")))
          (%make-lexical 'ATOM #t "#t"))))

    (define (%read-false-literal port)
      (read-char port)
      (let ((pc (peek-char port)))
        (if (char=? pc #\a)
          (begin
            (read-char port)
            (if
              (and (char=? (read-char port) #\l)
                   (char=? (read-char port) #\s)
                   (char=? (read-char port) #\e))
              #f
              (error "Invalid literal")))
          #f)))

    (define (read-sharp port)
      (read-char port)
      (let ((pc (peek-char port)))
        (case pc
          ((#\\) (%read-char-literal port))
          ((#\t) (%read-true-literal port))
          ((#\f) (%read-false-literal port))
          (else (error "WIP" pc)))))

    (define (read-u10integer port)
      (let loop ((res 0))
        (let ((pc (peek-char port)))
          (if (and (not (eof-object? pc)) (char-numeric? pc))
            (loop (+ (* 10 res) (- (char->integer (read-char port))
                                   (char->integer #\0))))
            res))))

    (define (read-u16bit-integer port)
      (let loop ((res 0))
        (let ((pc (peek-char port)))
          (case pc
            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
             (loop (+ (* 16 res) (- (char->integer (read-char port))
                                    (char->integer #\0)))))
            ((#\A #\B #\C #\D #\E #\F)
             (loop (+ (* 16 res) (+ (- (char->integer (read-char port))
                                       (char->integer #\A))
                                    10))))
            ((#\a #\b #\c #\d #\e #\f)
             (loop (+ (* 16 res) (+ (- (char->integer (read-char port))
                                       (char->integer #\a))
                                    10))))
            (else res)))))

    (define (%read-escape port)
      (let ((c (read-char port)))
        (case c
          ((#\") #\")
          ((#\\) #\\)
          ((#\x)
           ;;hex scalar value
            (let* ((res (read-u16bit-integer port))
                   (semi-colon-check (read-char port)))
              (unless (eq? semi-colon-check #\;)
                (error "Can't read \\x<hex scalar value>;"))
              (integer->char res)))
          ((#\|) #\|)
          ((#\a) #\alarm)
          ((#\b) #\backspace)
          ((#\t) #\tab)
          ((#\n) #\newline)
          ((#\r) #\return))))

    (define (read-string-literal port)
      (read-char port)
      (let loop ((res '()))
        (let ((c (read-char port)))
          (cond
            ((char=? c #\\)
             ;escape
             (loop (cons (%read-escape port) res)))
            ((char=? c #\") (list->string (reverse res)))
            (else (loop (cons c res)))))))

    (define (read-minus port)
      (read-char port)
      '-)

    (define (get-token port)
      (let ((pc (peek-char port)))
        (cond
          ((eof-object? pc) (eof-object))
          ((char=? pc #\() (read-pair port))
          ((char=? pc #\') (read-char port) (list 'quote (get-token port)))
          ((or (char-alphabetic? pc)
               (char-special-initial? pc))
           ;;<initial> <subsequent>*
            (read-identifier port))
          ((char=? pc #\|) (read-vertical-var-identifier port))
          ((char-numeric? pc) (read-u10integer port))
          ((char=? pc #\-) (read-minus port))
          ((or (eq? pc #\space)
               (eq? pc #\tab))
           (make-lexical 'SPACE (read-char port)))
          ((or (eq? pc #\newline)
               (eq? pc #\return))
           (make-lexical 'NEWLINE (read-char port)))
          ((char=? pc #\") (read-string-literal port))
          ((eq? pc #\#)
            ;;sharp-read
            (read-sharp port))
          ((eq? pc #\;)
           ;;read-oneline-comment
           (read-oneline-comment port))
          ((eq? pc #\|))
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
      (and (lexical? obj)
           (or (eq? (ref-type obj) 'SPACE)
               (eq? (ref-type obj) 'NEWLINE)
               (eq? (ref-type obj) 'COMMENT))))

    (define (remove-visual-literals obj)
      (cond
        ((list? obj)
         (map remove-visual-literals (remove visual-literal? obj)))
        ((pair? obj)
         (let ((head (remove-visual-literals (car obj)))
               (rest (remove-visual-literals (cdr obj))))
           (if (visual-literal? head)
             rest
             (cons head rest))))
        ((and (lexical? obj)
              (eq? (ref-type obj) 'ATOM))
         (ref-data obj))
        ((and (lexical? obj)
              (eq? (ref-type obj) 'DOT-PAIR))
         (let ((rests (remove-visual-literals (ref-data obj))))
           (car rests)))
        (else obj)))

    (define (read . args)
      (let ((port (or (and (not (null? args)) (car args))
                      (current-input-port))))
        (let ((res (remove-visual-literals (read-internal port))))
          (if (null? res)
            (eof-object)
            (car res)))))))
