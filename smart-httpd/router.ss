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
                 append-map)
        ./handler
        ./response
        ./resolution
        ./rejection)
(export #t)

(define-record-type <route-tree>
  (route-tree segment children handlers)
  route-tree?
  (segment    route-tree-segment)
  (children   route-tree-children)
  (handlers   route-tree-handlers))

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
          (when (= (length with-segments) 0)
            (display "bempty"))

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
