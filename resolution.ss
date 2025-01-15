(import :std/error
        :std/sugar
        :std/net/httpd
        :std/net/uri
        :std/text/json
        :std/misc/uuid
        :std/misc/func
        :std/misc/string
        :std/format
        :std/srfi/9
        :std/srfi/1
        :std/srfi/13)
(export #t)

(define-record-type <resolution>
  (resolution ok results)
  resolution?
  (ok      resolution-resolved?)
  (results resolution-results))
