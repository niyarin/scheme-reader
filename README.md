# Scheme-reader
WIP

## Usage
```scheme
(import (scheme base)
        (prefix (scheme-reader core) rdr/))

(rdr/read (open-input-string "(foo bar baz)"))  ;; => (foo bar baz)
```

