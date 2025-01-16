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
  (handler () <- (body :>)
           "Hello World!"))

(def test-handler-file
  (handler () <- (body :>)
           (displayln "bungus")
           (file-path "index.html")))

(def routes
  (list
   (get "/"       test-handler)
   (list
    (get "/bung"   test-handler)
    (get "/file"   test-handler-file))
   (get "/bung/b" test-handler)))

(define (main . args)
  (displayln "running on port 8080")
  (run-server routes port: 8080))
