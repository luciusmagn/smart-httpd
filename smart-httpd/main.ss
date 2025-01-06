;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/cli/getopt
        :std/net/httpd
        ./lib)
(export main)

(include "../manifest.ss")

(define (main)
  (displayln "bungus"))
