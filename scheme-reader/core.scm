(define-library (scheme-reader core)
  (cond-expand
    ((or chicken guile kawa)
      (import (scheme base)
              (only (srfi 1) remove)
              (scheme char)
              (scheme write)))
    (else
      (import (scheme base)
              (only (scheme list) remove)
              (scheme char)
              (scheme write))))
  (export read read-internal lexical? lexical-type lexical-data lexical-origin
          make-lexical
          read-internal-or-handle-shebang *tokenizer-mode* read-token)

  (begin
    (define use-guile-style-keyword (make-parameter #t))
    (define *tokenizer-mode* (make-parameter #f))

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

    (define (single-dot? obj)
      (and (lexical? obj)
           (eq? (ref-type obj) 'DOT)))

    (define (read-pair port)
      (read-char port)
      (let ((res-pair-head (list '())))
        (let loop ((res-pair res-pair-head))
          (let ((pc (peek-char port)))
            (cond
              ((char=? pc #\)) (read-char port) (cdr res-pair-head))
              ((char=? pc #\.)
               (let ((token (read-dot port)))
                 (if (single-dot? token)
                   (begin
                     (read-char port)
                     (let ((rests (list (%read-internal1-core port))))
                       (set-cdr! res-pair (make-lexical-dot-pair rests))
                       (loop rests)))
                   (begin
                    (set-cdr! res-pair (list '()))
                    (set-car! (cdr res-pair) token)
                    (loop (cdr res-pair))))))
              ((eof-object? pc) (error "READ END!"))
              (else
                (set-cdr! res-pair (list '()))
                (set-car! (cdr res-pair) (%read-internal1-core port))
                (loop (cdr res-pair))))))))

    (define (read-oneline-comment port)
      (read-char port)
      (let loop ((ls '()))
        (let ((c (peek-char port)))
          (if (or (char=? c #\newline)
                  (char=? c #\return))
            (make-lexical 'COMMENT (list->string (reverse ls)))
            (begin
              (read-char port)
              (loop (cons c ls)))))))

    (define (char-special-initial? c)
      (case c
        ((#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~) #t)
        (else #f)))

    (define (char-explicit-sign? c)
      (or (char=? c #\+) (char=? c #\-)))

    (define (char-special-subsequent? c)
      (and (not (eof-object? c))
           (or (char-explicit-sign? c)
               (char=? c #\.)
               (char=? c #\@))))

    (define (read-dot port)
      (read-char port)
      (let ((pc (peek-char port)))
        (case pc
          ;;TODO: dot subsequenct
          ((#\.);
           ;;TODO: subsequent
           (read-char port)
           (let loop ((ls '(#\. #\.)))
             (let ((pc* (peek-char port)))
               (if (and (char? pc*) (char=? pc* #\.))
                 (begin
                   (read-char port)
                   (loop (cons pc ls)))
                 (string->symbol (list->string (reverse ls)))))))
          (else (%make-lexical 'DOT "." ".")))))

    (define (%read-identifier-aux port)
      ;;read identifier as string
      (let loop ((ls '()))
        (let ((pc (peek-char port)))
          (if (and (not (eof-object? pc))
                   (or (char-special-initial? pc)
                       (char-special-subsequent? pc)
                       (char-alphabetic? pc)
                       (char-numeric? pc)))
            (loop (cons (read-char port) ls))
            (list->string (reverse ls))))))


    (define (read-identifier port)
      ;;<initial> <subsequent>*
      ;; NOTE: This procedure does not check whether the first character is <initial>.
      (string->symbol (%read-identifier-aux port)))

    (define (read-peculiar-identifier-or-signed-integer port)
      (let* ((first-char (read-char port))
             (pc (peek-char port)))
        (if (and (not (eof-object? pc)) (char-numeric? pc))
          (let ((num (read-u10integer port)))
            (case first-char
              ((#\+) num)
              ((#\-)
               (if (lexical? num)
                 (%make-lexical
                   'ATOM
                   (- (lexical-data num))
                   (string-append "-" (lexical-origin num)))
                 (- num)))))
            (string->symbol
              (string-append
                (string first-char)
                (%read-identifier-aux port))))))

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
        (if (and (not (eof-object? pc))
                 (char=? pc #\r))
          (begin
            (read-char port)
            (if (and (char=? (read-char port) #\u) (char=? (read-char port) #\e))
              (%make-lexical 'ATOM #t "#true")
              (error "Invalid literal.")))
          (%make-lexical 'ATOM #t "#t"))))

    (define (%read-false-literal port)
      (read-char port)
      (let ((pc (peek-char port)))
        (if (and (not (eof-object? pc))
                 (char=? pc #\a))
          (begin
            (read-char port)
            (if
              (and (char=? (read-char port) #\l)
                   (char=? (read-char port) #\s)
                   (char=? (read-char port) #\e))
              (%make-lexical 'ATOM #f "#false")
              (error "Invalid literal")))
          (%make-lexical 'ATOM #f "#f"))))

    (define (%read-guile-style-keyword port)
      (read-char port)
      (let ((keyw-symbol (read-identifier port)))
        ;;TODO: Save original code.
        (%make-lexical 'KEYWORD keyw-symbol keyw-symbol)))

    (define (%read-exactness port)
      (let ((pc (peek-char port)))
        (and (char=? pc #\#)
             (case (read-char port)
               ((#\i) 'inexactly)
               ((#\e) 'exactly)
               (else (error "Invalid exactness."))))))

    (define (%read-radix16-number port)
      (read-char port)
      (let ((exactness (%read-exactness port)))
        (read-u16bit-integer port)))

    (define (%read-datum-skip-comment port)
      (read-char port);;read #\;
      (let loop ((res '())
                 (origins "#!;"))
        (let ((obj (%read-internal1-core port)))
          (if (and (lexical? obj)
                   (or (eq? (lexical-type obj) 'SPACE)
                       (eq? (lexical-type obj) 'COMMENT)
                       (eq? (lexical-type obj) 'DIRECTIVE)))
            (let ((origin (ref-origin obj)))
              (loop (cons obj res)
                    (string-append
                      origins
                      (if origin origin ""))));;TODO: Add origin to directive.
            (%make-lexical
              'COMMENT
              (reverse res)
              origins)))))

    (define (%read-sharp-aux pc port)
      (case pc
          ((#\\) (%read-char-literal port))
          ((#\t) (%read-true-literal port))
          ((#\f) (%read-false-literal port))
          ((#\:)
           (if (use-guile-style-keyword)
             (%read-guile-style-keyword port)
             (error "WIP")))
          ((#\() (list->vector (read-pair port)))
          ((#\x);; <radix 16>
           (%read-radix16-number port))
          ((#\!)
           ;;read-internal-directive
           (read-char port)
           (make-lexical
             'DIRECTIVE
             (read-identifier port)))
          ((#\;)
           ;; datum skip comment
           (if (*tokenizer-mode*)
             (begin
               (read-char port)
               (make-lexical 'DATUM-SKIP-COMMENT-START "#;"))
             (%read-datum-skip-comment port)))
          (else (error "WIP" pc))))

    (define (read-sharp port)
      (read-char port)
      (let ((pc (peek-char port)))
        (%read-sharp-aux pc port)))

    (define (handle-shebang port)
      (read-char port)
      (let loop ((c (peek-char port))
                 (content '()))
        (if (char=? c #\newline)
          (let ((content* (list->string (reverse content))))
             (%make-lexical 'SHEBANG content* content*))
          (begin
            (read-char port)
            (loop (peek-char port) (cons c content))))))

    (define (read-internal-or-handle-shebang port)
      (let ((top (peek-char port)))
        (if (char=? top #\#)
          (begin
            (read-char port)
            (let ((next (peek-char port)))
              (if (char=? next #\!)
                (handle-shebang port)
                (%read-sharp-aux next port))))
          (read-internal port))))

    (define (%read-fractional-part port)
      (let loop ((res 0)
                 (unit 0.1))
        (let ((pc (peek-char port)))
          (cond
            ((and (not (eof-object? pc)) (char-numeric? pc))
             (loop (+ (* unit (- (char->integer (read-char port)) (char->integer #\0)))
                      res)
                   (* unit 0.1)))
            (else res)))))

    (define (read-u10integer port)
      (let loop ((res 0))
        (let ((pc (peek-char port)))
          (cond
            ((and (not (eof-object? pc)) (char-numeric? pc))
              (loop (+ (* 10 res) (- (char->integer (read-char port))
                                     (char->integer #\0)))))
            ((and (not (eof-object? pc)) (char=? pc #\.))
             (read-char port)
             (let ((fractal (%read-fractional-part port)))
               (if (zero? fractal)
                 (%make-lexical
                   'ATOM
                   (+ fractal res)
                   (string-append
                     (number->string res)
                     ".0"))
                 (+ fractal res))))
            (else res)))))

    (define (read-u16bit-integer port)
      (let loop ((res 0)
                 (org '()))
        (let ((pc (peek-char port)))
          (case pc
            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
             (loop (+ (* 16 res) (- (char->integer (read-char port))
                                    (char->integer #\0)))
                   (cons pc org)))
            ((#\A #\B #\C #\D #\E #\F)
             (loop (+ (* 16 res) (+ (- (char->integer (read-char port))
                                       (char->integer #\A))
                                    10))
                   (cons pc org)))
            ((#\a #\b #\c #\d #\e #\f)
             (loop (+ (* 16 res) (+ (- (char->integer (read-char port))
                                       (char->integer #\a))
                                    10))
                   (cons pc org)))
            (else (values res org))))))

    (define (single-char-escape? c)
      (case c
        ((#\") #\")
        ((#\\) #\\)
        ((#\|) #\|)
        ((#\a) #\alarm)
        ((#\b) #\backspace)
        ((#\t) #\tab)
        ((#\n) #\newline)
        ((#\r) #\return)
        (else #f)))

    (define (%read-escape port)
      (let ((c (read-char port)))
        (cond
          ((single-char-escape? c)
          => (lambda (v)
               (values v (list c))))
          ((char=? c #\x)
           ;;hex scalar value
           (let-values (((res rorg) (read-u16bit-integer port)))
            (let* ((semi-colon-check (read-char port)))
              (unless (eq? semi-colon-check #\;)
                (error "Can't read \\x<hex scalar value>;"))
              (values (integer->char res)
                      (append '(#\;) rorg  '(#\x #\\) )))))
         (else (values #f #f)))))

    (define (intraline-whitespace-char? c)
      (or (char=? c #\space)
          (char=? c #\tab)))

    (define (%read-line-break c port)
      ;; return reversed original characters
      (let loop ((c c)
                 (raw '()))
        (case c
          ((#\newline) (cons c raw))
          ((#\space #\tab) (loop (read-char port) (cons c raw)))
          ((#\return)
           (let ((nc (peek-char port)))
             (if (char=? #\newline nc)
               (begin (read-char port)
                      (cons nc (cons c raw)))
               (cons c raw)))))))


    ;;TODO: use <literal> and maintain original code to calc line number and col.
    (define (read-string-literal port)
      (read-char port)
      (let loop ((res '())
                 (original '(#\")))
        (let* ((c (read-char port))
               (nc (peek-char port)))
          (cond
            ((and (char=? c #\\)
                  (or (intraline-whitespace-char? nc)
                      (char=? nc #\newline)
                      (char=? nc #\return))
                  (%read-line-break (read-char port) port))
             => (lambda (reversed-spaces)
                   (loop res
                         (append reversed-spaces (cons c original)))))
            ((char=? c #\\)
             (let-values ((( rc roriginal) (%read-escape port)))
             ;escape
             (loop (cons rc res)
                   (append roriginal original))))
            ((char=? c #\")
              (%make-lexical
                'STRING
                (list->string (reverse res))
                (list->string (reverse (cons #\" original)))))
            (else (loop (cons c res)
                        (cons c original)))))))

    (define (read-minus port)
      (read-char port)
      '-)

    (define (read-unquotes port)
       (read-char port)
       (if (char=? (peek-char port) #\@)
         (begin
           (read-char port)
           (list 'unquote-splicing (%read-internal1-core port)))
         (list 'unquote (%read-internal1-core port))))

    (define (%read-internal1-core port)
      (let ((pc (peek-char port)))
        (cond
          ((eof-object? pc) (eof-object))
          ((char=? pc #\()
           (if (*tokenizer-mode*)
             (make-lexical 'OPEN-PAREN (read-char port))
             (read-pair port)))
          ((char=? pc #\))
           (if (*tokenizer-mode*)
             (make-lexical 'CLOSE-PAREN (read-char port))
             (error "Invalid closed paren error.")))
          ;TODO: Use <lexical>.
          ((char=? pc #\')
           (if (*tokenizer-mode*)
             (make-lexical 'QUOTE (read-char port))
             (begin
               (read-char port)
               (list 'quote (%read-internal1-core port)))))
          ((char=? pc #\`)
           (if (*tokenizer-mode*)
             (make-lexical 'QUASI-QUOTE (read-char port))
             (begin
                (read-char port)
                (list 'quasiquote (%read-internal1-core port)))))
          ((char=? pc #\,)
           (if (*tokenizer-mode*)
             (make-lexical 'UNQUOTE (read-char port))
             (read-unquotes port)))

          ((char-explicit-sign? pc)
           ;;number or identifier
           (read-peculiar-identifier-or-signed-integer port))
          ((or (char-alphabetic? pc)
                 (char-special-initial? pc))
           ;;<initial> <subsequent>*
            (read-identifier port))
          ((char=? pc #\.) (read-dot port))
          ((char=? pc #\|) (read-vertical-var-identifier port))
          ((char-numeric? pc) (read-u10integer port))
          ((char=? pc #\-) (read-minus port))
          ((intraline-whitespace-char? pc)
           (%make-lexical 'SPACE (read-char port) (string pc)))
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
          (else (read-char port)))))

    (define (read-internal port)
      (let loop ((res '()))
        (let ((tkn (%read-internal1-core port)))
          (if (and (lexical? tkn)
                   (or (eq? (ref-type tkn) 'SPACE)
                       (eq? (ref-type tkn) 'NEWLINE)
                       (eq? (ref-type tkn) 'COMMENT)
                       (eq? (ref-type tkn) 'DIRECTIVE)))
            (loop (cons tkn res))
            (reverse (cons tkn res))))))

    (define (read-token port)
      (parameterize ((*tokenizer-mode* #t))
        (%read-internal1-core port)))

    (define (visual-literal? obj)
      (and (lexical? obj)
           (or (eq? (ref-type obj) 'SPACE)
               (eq? (ref-type obj) 'NEWLINE)
               (eq? (ref-type obj) 'COMMENT)
               (eq? (ref-type obj) 'DIRECTIVE))))

    (define (%directive-literal? obj)
      (and (lexical? obj)
           (eq? (ref-type obj) 'DIRECTIVE)))

    (define (%fold-case-directive? obj)
      (and (%directive-literal? obj)
           (eq? (ref-data obj) 'fold-case)))

    (define (remove-visual-literals obj case-fold-flag)
      (cond
        ;((list? obj)
        ; (map (lambda (o) (remove-visual-literals o case-fold-flag))
        ;      (remove visual-literal? obj)))
        ((pair? obj)
         (let* ((head (remove-visual-literals (car obj) case-fold-flag))
                (rest (remove-visual-literals (cdr obj)
                                              (if (%fold-case-directive? head) #t case-fold-flag))))
           (if (visual-literal? head)
             rest
             (cons head rest))))
        ((vector? obj)
         (list->vector
           (map (lambda (o) (remove-visual-literals o case-fold-flag))
                (remove visual-literal? (vector->list obj)))))

        ((and (lexical? obj)
              (eq? (ref-type obj) 'ATOM))
         (ref-data obj))
        ((and (lexical? obj)
              (eq? (ref-type obj) 'STRING))
         (ref-data obj))
        ((and (lexical? obj)
              (eq? (ref-type obj) 'DOT-PAIR))
         (let ((rests (remove-visual-literals (ref-data obj) case-fold-flag)))
           (car rests)))
        ((and case-fold-flag
              (symbol? obj))
         (string->symbol (string-foldcase (symbol->string obj))))
        (else obj)))

    (define (read . args)
      (let* ((port (or (and (not (null? args)) (car args))
                      (current-input-port)))
             (res (remove-visual-literals (read-internal port) #f)))
          (if (null? res)
            (eof-object)
            (car res))))))
