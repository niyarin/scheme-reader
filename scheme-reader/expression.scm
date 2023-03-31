(define-library (scheme-reader expression)
  (import (scheme base))
  (export make-lexical lexical? ref-type ref-data)
  (begin
    ;;まだドット対は未サポート
    (define-record-type <lexical>
      (make-lexical type data)
      lexical?
      (type ref-type)
      (data ref-data))
    ))

