;;; -*- Gerbil -*-
(import :std/sugar
        :std/format
        :std/misc/hash
        :std/srfi/9
        :std/srfi/13
        :lho/fxns/lib)
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

(fn :ret parse-cookie ((str : string?) -> request-cookie?)
    (let* ((parts (string-split str #\=))
           (name (string-trim (car parts)))
           (value (string-trim (cadr parts))))
      (make-request-cookie name value)))

(fn :ret parse-cookies ((str : string?) -> (list-of request-cookie?))
    (map parse-cookie
         (filter (lambda (s) (string-contains s "="))  ; skip empty parts
                 (string-split str #\;))))

(fn :ret print-cookie ((cookie : [request-cookie? set-cookie?]) -> void?)
    (cond
     ((request-cookie? cookie)
      (printf "name: ~a\n" (request-cookie-name cookie))
      (printf "value: ~a\n" (request-cookie-value cookie)))
     ((set-cookie? cookie)
      (printf "name: ~a\n" (set-cookie-name cookie))
      (printf "value: ~a\n" (set-cookie-value cookie))
      (printf "attrs: ~a\n" (set-cookie-attrs cookie))
      (printf "flags: ~a\n" (set-cookie-flags cookie)))))



(fn :ret find-cookie ((cookies : (list-of request-cookie?)) (name : string?) -> (or-false? request-cookie?))
    (find (lambda (c)
            (equal? name (request-cookie-name c)))
          cookies))

(fn :ret find-cookie-val ((cookies : (list-of request-cookie?)) (name : string?) -> (or-false? string?))
    (let (ck (find-cookie cookies name))
      (if ck
        (request-cookie-value ck)
        #f)))

(fn :ret set-cookie->string ((cookie : set-cookie?) -> string?)
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
