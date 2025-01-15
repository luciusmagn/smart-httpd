#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("smart-httpd/lib"
    "smart-httpd/cookie"
    "smart-httpd/conversions"
    "smart-httpd/rejection"
    "smart-httpd/server"
    "smart-httpd/router"
    "smart-httpd/response"
    "smart-httpd/resolution"
    "smart-httpd/handler"
    (exe: "smart-httpd/main" bin: "smart-httpd")))
