;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/net/httpd
        :std/net/uri
        :std/text/json
        :std/misc/uuid
        :std/misc/func
        :std/misc/string
        :std/format
        :std/srfi/9
        :std/srfi/1
        :std/srfi/13
        ./conversions
        ./rejection
        ./resolution
        ./cookie)
(export #t)

(define-record-type <file-path>
  (file-path path)
  file-path?
  (path file-path-get))

(define-record-type <response>
  (make-response body headers status)
  response?
  (body     response-body)
  (headers  response-headers)
  (status   response-status-code))

(define-record-type <response-body>
  (body content)
  response-body?
  (content response-body-content))

(define-record-type <response-header>
  (header name value)
  response-header?
  (name response-header-name)
  (value response-header-value))

(define-record-type <response-status>
  (status code)
  response-status?
  (code response-status))

(define (cookie name value)
  ;; TODO: Set reasonable defaults, allow adding attributes and flags
  (let ((cookie (make-set-cookie name value '() '())))
    (header "Set-Cookie" (set-cookie->string cookie))))

(define (respond-with . forms)
  (let loop ((forms forms)
             (body "")
             (headers '())
             (status 200))
    (displayln forms)
    (if (null? forms)
      (begin
        (displayln "finished response")
        (make-response body headers status))
      (let ((form (car forms))
            (rest (cdr forms)))
        (cond
         ((response-body? form)
          (displayln "write body")
          (loop rest (response-body-content form) headers status))
         ((response-header? form)
          (displayln "write header")
          (loop rest body
                (cons (cons (response-header-name form)
                            (response-header-value form))
                      headers)
                status))
         ((response-status? form)
          (displayln "write status")
          (loop rest body headers (response-status form))))))))

(define (process-response resolve response res)
  (cond
   ((string? response)
    (http-response-write res 200 '() response)
    (resolve (resolution #t '())))
   ((file-path? response)
    (http-response-file res '() (file-path-get response))
    (resolve (resolution #t '())))
   ((pair? response)
    (if (list? response)
      ;; (status headers body)
      (let ((status   (car   response))
            (headers  (cadr  response))
            (body     (caddr response)))
        (http-response-write res status headers body))
      ;; (status . body)
      (let ((status   (car   response))
            (body     (cdr   response)))
        (http-response-write res status '() body)))
    (resolve (resolution #t '())))
   ((response? response)
    (displayln "writing response")
    (http-response-write res
                         (response-status-code response)
                         (response-headers     response)
                         (response-body        response))
    (resolve (resolution #t '())))))
