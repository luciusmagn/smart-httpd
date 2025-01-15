#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("smart-httpd/lib"
    "smart-httpd/cookie"
    "smart-httpd/conversions"
    "smart-httpd/rejection"
    (exe: "smart-httpd/main" bin: "smart-httpd")))
