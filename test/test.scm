(import (scheme base)
        ;(scheme write)
        (srfi 78)
        (prefix (scheme-reader core) rdr/))


;;literal test
(check (rdr/read (open-input-string "some-symbol"))
       => 'some-symbol)

(check-report)

