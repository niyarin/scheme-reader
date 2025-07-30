# Scheme-reader
This library provides a portable implementation of a Scheme reader.
It can also produce an intermediate representation that preserves the original source notation and includes line number information.


## Usage
```scheme
(import (scheme base)
        (prefix (scheme-reader core) rdr/))

(rdr/read (open-input-string "(foo bar baz)"))  ;; => (foo bar baz)
```

