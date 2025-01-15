;;; -*- Gerbil -*-
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
        :std/srfi/13
        ./router)
(export #t)

(define (run-server routes . args)
  (define port (pget port: args 8080))
  (define address (pget address: args "127.0.0.1"))
  (define static (pget static: args 'default))
  (define recovery (pget recovery: args 'default))

  (let* ((addr (format "~a:~a" address port))
         (handler (router static recovery routes))
         (server (start-http-server!
                  addr
                  mux: (make-default-http-mux handler))))
    (thread-join! server)))
