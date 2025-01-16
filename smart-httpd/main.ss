;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/cli/getopt
        :std/net/httpd
        ./lib)
(export main)

(include "../manifest.ss")

(define test-handler
  (handler () <- (body :>)
           "Hello World!"))

(define test-handler-file
  (handler () <- (body :>)
           (displayln "bungus")
           (file-path "index.html")))

(define plus-route
  ;; GET /plus/:x/:y
  (handler ((x :>number) (y :>number)) <- (_ :>)
           (displayln (+ x y))
           (number->string (+ x y))))

(define respond-with-handler
  (handler () <- (_ :>)
           (respond-with
            (status 200)
            (body   "Hello World! This time combined"))))

(def routes
  (list
   (get "/"           test-handler)
   (list
    (get "/bung"      test-handler)
    (get "/file"      test-handler-file))
   (get "/bung/b"     test-handler)
   (get "/plus/:x/:y" plus-route)
   (get "/respond"    respond-with-handler)))

(define (main . args)
  (displayln "running on port 8080")
  (run-server routes port: 8080))
