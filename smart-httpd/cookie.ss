;;; -*- Gerbil -*-
(import :std/sugar
        :std/misc/hash
        :std/srfi/9
        :std/srfi/13)
(export #t)

(define-record-type <request-cookie>
  (make-request-cookie name value)
  request-cookie?
  (name  request-cookie-name)
  (value request-cookie-value))

(define-record-type <set-cookie>
  (make-set-cookie name value attrs flags)
  set-cookie?
  (name  set-cookie-name)
  (value set-cookie-value)
  (attrs set-cookie-attrs)
  (flags set-cookie-flags))

(def (parse-cookie str)
  (let* ((parts (string-split str #\=))
         (name (string-trim (car parts)))
         (value (string-trim (cadr parts))))
    (make-request-cookie name value)))

(def (parse-cookies str)
  (map parse-cookie
       (filter (lambda (s) (string-contains s "="))  ; skip empty parts
               (string-split str #\;))))

(def (set-cookie->string cookie)
  (let ((parts (list (string-append
                      (set-cookie-name cookie)
                      "="
                      (set-cookie-value cookie)))))
    ;; Add attributes
    (hash-for-each
     (lambda (k v)
       (set! parts (cons (string-append k "=" v) parts)))
     (set-cookie-attrs cookie))
    ;; Add flags
    (for-each
      (lambda (flag)
        (set! parts (cons (symbol->string flag) parts)))
      (set-cookie-flags cookie))
    ;; Join with semicolons
    (string-join (reverse parts) "; ")))
