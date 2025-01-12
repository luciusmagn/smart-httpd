#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("smart-httpd/lib" "smart-httpd/cookie"
    (exe: "smart-httpd/main" bin: "smart-httpd")))
