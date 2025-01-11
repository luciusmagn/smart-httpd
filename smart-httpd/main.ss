;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/cli/getopt
        :std/net/httpd
        ./lib)
(export main)

(include "../manifest.ss")

(def (greet name . args)
  (def greeting (pget greeting: args "Hello"))
  (string-append greeting " " name "!"))

(def test-handler
  (handler () <- (body :>json)
           "Hello World!"))

(def routes
  (list
   (get "/" test-handler)))

(define (main . args)
  (displayln "running on port 8080")
  (run-server routes port: 8080))
