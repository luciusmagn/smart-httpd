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
        :std/srfi/13
        (only-in :std/srfi/1
                 every
                 remove
                 append-map
                 zip)
        ./handler
        ./response
        ./resolution
        ./rejection)
(export #t)

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

(define (route-matches? route path)
  (let ((route-segments (vector->list (car route)))
        (path-segments  (parse-path path)))
    (call/cc
      (lambda (return)
        (when (not (= (length route-segments)
                      (length path-segments)))
          (return #f))

        (define (match-segment seg1 seg2)
          (or (segment-dynamic? seg1)
              (and (segment-exact? seg1)
                   (segment-exact? seg2)
                   (string=? (segment-exact-name seg1)
                             (segment-exact-name seg2)))))

        (printf "zipped length: ~a\n" (length (zip route-segments path-segments)))
        (every (lambda (pair) (apply match-segment pair))
               (zip route-segments path-segments))))))

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

  (let* ((routes           (map vector->list (flatten-rec routes)))
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
        (let* ((matching-routes      (filter (lambda (route)
                                               (route-matches? route path))
                                             routes))
               (handlers             (map cadr matching-routes))
               (by-method            (filter (lambda (spec)
                                               (eq? method (handler-spec-method spec)))
                                             handlers))
               (by-headers           (filter (lambda (spec)
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
          (when (= (length with-segments) 0)
            (displayln "No initially matching handlers"))

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
