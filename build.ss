#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("smart-httpd/lib"
    (exe: "smart-httpd/main" bin: "smart-httpd")))
