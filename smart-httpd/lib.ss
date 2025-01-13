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
        ./cookie)
(export #t)

(define-record-type <rejection>
  (rejection type msg)
  rejection?
  (type      rejection-type)
  (msg       rejection-msg))

(define-record-type <file-path>
  (file-path path)
  file-path?
  (path file-path-get))

(define-record-type <extractor>
  (extractor type fn name)
  extractor?
  ;; symbol 'segment 'headers 'body
  (type  extractor-type)
  (fn    extractor-fn)
  (name  extractor-name))

(defrules define-segment-extractor ()
  ((define-segment-extractor name fn)
   (define name (extractor 'segment fn (symbol->string 'name)))))

(defrules define-headers-extractor ()
  ((define-segment-extractor name fn)
   (define name (extractor 'headers fn (symbol->string 'name)))))

(defrules define-body-extractor ()
  ((define-segment-extractor name fn)
   (define name (extractor 'body    fn (symbol->string 'name)))))

(define (segment-extractor? extractor)
  (eq? 'segment (extractor-type extractor)))

(define (headers-extractor? extractor)
  (eq? 'headers (extractor-type extractor)))

(define (body-extractor? extractor)
  (eq? 'body    (extractor-type extractor)))

;; handler conversions
;; :> is identity, because we always need something
(define-segment-extractor  :>            (lambda (x) x))
(define-segment-extractor  :>string      :>) ; yeah
(define-segment-extractor  :>number      string->number)
(define-segment-extractor  :>keyword     string->keyword)
(define-segment-extractor  :>symbol      string->symbol)
(define-segment-extractor  :>uuid        string->uuid)

(define-body-extractor
  :>json
  (lambda (body)
    (try
     (if (string? body)
       (call-with-input-string body read-json)
       (rejection 'invalid-json "Body is not a valid string"))
     (catch (e)
       (rejection 'invalid-json "Failed to parse JSON")))))

(define-headers-extractor :>cookies parse-cookies)


(define-body-extractor
  :>form
  (lambda (body)
    (try
     (if (string? body)
       (let ((pairs (form-url-decode body)))
         (let ((tab (make-hash-table)))
           (for-each (lambda (pair)
                       (hash-put! tab
                                  (string->symbol (car pair))
                                  (cdr pair)))
                     pairs)
           tab))
       (rejection 'invalid-form "Body is not a valid string"))
     (catch (e)
       (rejection 'invalid-form "Failed to parse form data")))))

(define table-example
  (handler () <- (body :>json)
           (let ((name (hash-ref body 'name))
                 (age  (hash-ref body 'age)))
             (string-append "Hello " name ", you are " (number->string age)))))

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

(define add-two
  (handler ((x :>number) (y :>number)) <- (_ :>)
           (displayln x)
           (displayln y)
           (+ x y)))

(define-record-type <route-tree>
  (route-tree segment children handlers)
  route-tree?
  (segment    route-tree-segment)
  (children   route-tree-children)
  (handlers   route-tree-handlers))

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

(define (build-route-tree routes)
  (define (segments-equal? seg1 seg2)
    (cond
     ((and (segment-exact? seg1) (segment-exact? seg2))
      (string=? (segment-exact-name seg1)
                (segment-exact-name seg2)))
     ((and (segment-dynamic? seg1) (segment-dynamic? seg2))
      (string=? (segment-dynamic-name seg1)
                (segment-dynamic-name seg2)))
     (else #f)))

  (define (insert-route tree segments handler)
    (if (null? segments)
      (route-tree
       (route-tree-segment tree)
       (route-tree-children tree)
       (cons handler (route-tree-handlers tree)))
      (let* ((seg (car segments))
             (matching-child
              (find (lambda (child)
                      (segments-equal?
                       (route-tree-segment child)
                       seg))
                    (route-tree-children tree))))
        (route-tree
         (route-tree-segment tree)
         (if matching-child
           (cons (insert-route matching-child
                               (cdr segments)
                               handler)
                 (remove (lambda (el) (equal? el matching-child))
                         (route-tree-children tree)))
           (cons (make-leaf seg
                            (cdr segments)
                            handler)
                 (route-tree-children tree)))
         (route-tree-handlers tree)))))

  (define (make-leaf segment rest handler)
    (if (null? rest)
      (route-tree segment '() (list handler))
      (route-tree segment
                  (list (make-leaf (car rest)
                                   (cdr rest)
                                   handler))
                  '())))

  (foldl (lambda (route tree)
           (insert-route tree
                         (car route)
                         (cadr route)))
         (route-tree #f '() '())
         routes))

(define (print-route-tree routes)
  (define (padding level)
    (list->string (repeat #\space (* level 2))))

  (define (segment->string segment)
    (cond
     ((not segment) "#f")
     ((segment-exact? segment)
      (format "exact:~a" (segment-exact-name segment)))
     ((segment-dynamic? segment)
      (format "dynamic:~a" (segment-dynamic-name segment)))
     (else "invalid-segment")))

  (define (tree-iter node level)
    (printf "~asegment: ~a\n"
            (padding level)
            (segment->string (route-tree-segment node)))
    (printf "~ahandlers: ~a\n"
            (padding level)
            (route-tree-handlers node))
    (printf "~achildren: [~a]\n"
            (padding level)
            (length  (route-tree-children node)))
    (for-each
      (lambda (n) (tree-iter n (+ 1 level)))
      (route-tree-children node)))

  (tree-iter routes 0))

(define (user-handler)  (void))
(define (posts-handler) (void))

(define test-treee (build-route-tree
                    `(((,(segment-exact "users")
                        ,(segment-dynamic "id"))
                       ,user-handler)
                      ((,(segment-exact "posts"))
                       ,posts-handler))))


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

(define test-tree2 (build-route-tree
                    (list
                     (get  "/users/:id" user-handler)
                     (post "/users/:id" user-handler)
                     (get  "/posts"     posts-handler)
                     (post "/"          user-handler))))

(define (find-handlers tree path)
  (define (match-segment seg1 seg2)
    (or (segment-dynamic? seg1)
        (and (segment-exact? seg1)
             (segment-exact? seg2)
             (string=? (segment-exact-name seg1)
                       (segment-exact-name seg2)))))

  (define (traverse node segments accum-handlers)
    (cond
     ;; at end of path, return all handlers
     ((null? segments)
      (append (route-tree-handlers node) accum-handlers))

     ;; try all matching children
     (else
      (let* ((seg (car segments))
             (matching-children
              (filter (lambda (child)
                        (match-segment (route-tree-segment child) seg))
                      (route-tree-children node))))
        ;; recurse on all matching paths
        (append-map (lambda (child)
                      (traverse child
                                (cdr segments)
                                accum-handlers))
                    matching-children)))))

  (traverse tree (parse-path path) '()))

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

(define (error-template status message)
  (string-append
   "<!DOCTYPE html><html><head><style>
     body{font-family:sans-serif;height:100vh;margin:0;
          display:flex;flex-direction:column;
          justify-content:center;align-items:center}
     h1{margin:0}
     p{color:#666}
   </style></head>
   <body><h1>" status "</h1><p>" message "</p></body></html>"))

;; routes is a list of list|route (will be recursively flattened)
;; static-handler is either 'default, or a handler that takes a path
;; recovery is either 'default, or a function that takes a rejection, and produces a response
(define (router static recovery routes)
  (define (subset? list1 list2)
    (every
     (lambda (x)
       (member x list2))
     list1))

  (define (flatten-rec lst)
    (cond
     ((null? lst) '())
     ;; If it's a route (list of 2 elements), keep it intact
     ((and (pair? lst)
           (= (length lst) 2)
           (list? (car lst)))
      (list lst))
     ;; Otherwise recurse into nested lists
     ((pair? (car lst))
      (append (flatten-rec (car lst))
              (flatten-rec (cdr lst))))
     (else
      (cons (car lst)
            (flatten-rec (cdr lst))))))

  (define (default-static-handler path)
    (sanitize-static-path path))

  (define (default-recovery rejection)
    (error-template
     (symbol->string (car rejection))
     (cdr rejection)))

  (define-record-type <resolution>
    (resolution ok results)
    resolution?
    (ok      resolution-resolved?)
    (results resolution-results))

  (let* ((routes           (flatten-rec routes))
         (route-tree       (build-route-tree routes))
         (static-handler   (if (eq? 'default static)
                             default-static-handler
                             static))
         (recovery-handler (if (eq? 'default recovery)
                             default-recovery
                             recovery)))
    (eprintf "using default static handler: ~a\n"
             (eq? 'default static))
    (eprintf "using default recovery handler: ~a\n"
             (eq? 'default recovery))

    (lambda (req res)
      (let ((path     (http-request-path    req))
            (method   (http-request-method  req))
            (headers  (http-request-headers req))
            (body     (http-request-body    req)))
        (let* ((handlers      (find-handlers route-tree path))
               (by-method     (filter (lambda (spec)
                                        (eq? method (handler-spec-method spec)))
                                      handlers))
               (by-headers    (filter (lambda (spec)
                                        (subset? (handler-spec-headers spec) headers))
                                      by-method))
               ;; we build here pairs of (handler-spec . '(active-segments body))
               (with-segments (map    (lambda (spec)
                                        (cons spec
                                              (list (extract-params (handler-spec-path spec)
                                                                    path)
                                                    body
                                                    headers)))
                                      by-headers)))

          (define (exec-handlers)
            (call/cc
              (lambda (resolve)
                (define (iterate handler-pair)
                  (let* ((spec    (car handler-pair))
                         (handler (handler-spec-handler spec))
                         (params  (cdr handler-pair)))
                    (call/cc
                      (lambda (continue)
                        (let ((result (apply handler params)))
                          (when (rejection? result)
                            (eprintf "REJECT: ~a\n -> ~a : ~a\n"
                                     (spec->string    spec)
                                     (rejection-type result)
                                     (rejection-msg  result)
                                     (continue)))
                          (eprintf "ACCEPT: ~a\n" (spec->string spec))
                          ;; a handler may return
                          ;; - a string
                          ;; - a file-path
                          ;; - (status . body)
                          ;; - (status headers body)
                          ;; - a rejection
                          (cond
                           ;; string
                           ((string?  result)
                            (http-response-write res 200 '() result)
                            ;; we responded with a string, bail!
                            (resolve (resolution #t '())))
                           ;; file path
                           ((file-path? result)
                            (http-response-file res '() (file-path-get result))
                            ;; we responded with a file, bail!
                            (resolve (resolution #t '())))
                           ;; pair or list
                           ((pair? result)
                            (if (list? result)
                              ;; (status headers body)
                              (let ((status   (car   result))
                                    (headers  (cadr  result))
                                    (body     (caddr result)))
                                (http-response-write res status headers body))
                              ;; (status . body)
                              (let ((status   (car   result))
                                    (body     (cdr   result)))
                                (http-response-write res status '() body)))
                            ;; we responded with a status and possibly headers, bail!
                            (resolve (resolution #t '())))))))))
                ;; we return from the continuation on success
                ;; so we resolve with a failure if we didnt
                (let ((rejections (map iterate with-segments)))
                  (resolve (resolution #f rejections))))))
          ;; if no handler was good
          ;; (either because all failed or none were valid)
          ;; we try the static file handler
          ;;
          ;; if that does not work either, we show 404
          (let ((reso (exec-handlers)))
            (eprintf "resolution: ~a ~a\n" (resolution-resolved? reso) (resolution-results reso))
            (unless (resolution-resolved? reso)
              (let ((static-result (static-handler path)))
                (if (rejection? static-result)
                  (let ((error-response (recovery-handler (rejection 'not-found
                                                                     "No response was found to your request"))))
                    (http-response-write res 404 '() error-response))
                  ;; Actually serve the file if we got a valid file-path
                  (http-response-file res '() (file-path-get static-result)))))))))))

(define (run-server routes . args)
  (define port (pget port: args 8080))
  (define address (pget address: args "127.0.0.1"))
  (define static (pget static: args 'default))
  (define recovery (pget recovery: args 'default))

  (let* ((addr (format "~a:~a" address port))
         (handler (router static recovery routes))
         (server (start-http-server!
                  addr
                  mux: (make-default-http-mux handler))))
    (thread-join! server)))
