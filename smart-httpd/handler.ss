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
        :std/srfi/13
        ./conversions
        ./response
        ./rejection)
(export #t)

(define (extract-params pattern url)
  (define pat-parts
    (filter (lambda (x) (not (equal? x "")))
            (string-split pattern #\/)))
  (define url-parts
    (filter (lambda (x) (not (equal? x "")))
            (string-split url #\/)))

  (if (not (= (length pat-parts) (length url-parts)))
    (rejection 'mismatched "Wrong number of URL segments")
    (filter-map (lambda (pat url-part)
                  (and (char=? (string-ref pat 0) #\:)
                       url-part))
                pat-parts
                url-parts)))

(define (sanitize-static-path url-path)
  (let ((cleaned (string-trim-prefix "/static/" url-path)))
    (if (string-contains cleaned "..")
      #f ; reject path traversal attempts
      (file-path
       (string-append "./static/" cleaned)))))

(define-record-type <segment-exact>
  (segment-exact   name)
  segment-exact?
  (name          segment-exact-name))

(define-record-type <segment-dynamic>
  (segment-dynamic name)
  segment-dynamic?
  (name            segment-dynamic-name))

(define (segment-type segment)
  (cond
   ((segment-exact?   segment) 'exact)
   ((segment-dynamic? segment) 'dynamic)
   (else #f)))

(define (parse-path path)
  (define (parse segment)
    (if (string-prefix? ":" segment)
      (segment-dynamic segment)
      (segment-exact   segment)))

  (let* ((cleaned  (if (string-prefix? "/" path)
                     (list->string (cdr (string->list path)))
                     path))
         (segments (string-split cleaned #\/))
         (parsed   (map parse segments)))
    parsed))

(define (print-path path)
  (if (null? path)
    (displayln "end")
    (let ((current (car path)))
      (cond
       ((segment-exact?   current)
        (display   "  exact: ")
        (displayln (segment-exact-name   current)))

       ((segment-dynamic? current)
        (display   "dynamic: ")
        (displayln (segment-dynamic-name current)))

       (else (displayln "invalid segment")))

      (print-path (cdr path)))))

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
