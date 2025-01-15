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
        ./conversions)
(export #t)

(define-record-type <handler-spec>
  (handler-spec method path headers handler)
  handler-spec?
  (method  handler-spec-method)
  (path    handler-spec-path)
  (headers handler-spec-headers)
  (handler handler-spec-handler))

(define (spec->string spec)
  (call-with-output-string
   (lambda (port)
     (display (handler-spec-method spec) port)
     (display " " port)
     (display (handler-spec-path   spec) port)
     (display " " port)
     (display (string-join (map (lambda (h) (format "~a" h)) (handler-spec-headers spec))) port))))

(define (define-route method path headers handler)
  (list (parse-path path) (handler-spec method path headers handler)))

(define (method-helper method)
  (case-lambda
    ((path handler headers) (define-route method path headers handler))
    ((path handler)         (define-route method path '() handler))))

(define get    (method-helper 'GET))
(define put    (method-helper 'PUT))
(define post   (method-helper 'POST))
(define patch  (method-helper 'PATCH))
(define delete (method-helper 'DELETE))

(defrules handler (<-)
  ((handler ((var conv) ...) <- (body bconv) statements statements* ...)
   (lambda (active-segments body-data headers)
     (def ptr
       (list-copy active-segments))

     (def (pop-ptr)
       (let ((elem (car ptr)))
         (set! ptr (cdr ptr))
         elem))

     (call/cc
       (lambda (reject)
         (define (validate converted)
           (cond
            ((boolean?   converted) (when (not converted) (reject converted)))
            ((rejection? converted) (reject converted))
            (else converted)))

         (try
          (let ((var (cond
                      ((segment-extractor? conv) (validate ((extractor-fn conv) (pop-ptr))))
                      ((headers-extractor? conv) (validate ((extractor-fn conv) (list headers))))
                      ((body-extractor? conv)    (validate ((extractor-fn conv) body-data)))
                      (else
                       (reject (rejection 'invalid-conv "Invalid conversion"))))) ...)
            (let ((body (if (string? body-data)
                          ((extractor-fn bconv) body-data)
                          ((extractor-fn bconv) ""))))
              statements
              statements* ...))
          (catch (e) (reject (rejection 'exception "An exception was caught")))))))))
