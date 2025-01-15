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
        ./cookie
        ./response
        ./router
        ./handler)

(export (import: ./rejection))
(export (import: ./conversions))
(export (import: ./response))
(export (import: ./router))
(export (import: ./cookie))
(export (import: ./handler))
(export #t)


;;(define add-two
;;  (handler ((x :>number) (y :>number)) <- (_ :>)
;;           (displayln x)
;;           (displayln y)
;;           (+ x y)))

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

;;(define (user-handler)  (void))
;;(define (posts-handler) (void))
;;
;;(define test-treee (build-route-tree
;;                    `(((,(segment-exact "users")
;;                        ,(segment-dynamic "id"))
;;                       ,user-handler)
;;                      ((,(segment-exact "posts"))))))
;;                       ,posts-handler))))
;;
;;(define test-tree2 (build-route-tree
;;                    (list
;;                     (get  "/users/:id" user-handler)
;;                     (post "/users/:id" user-handler)
;;                     (get  "/posts"     posts-handler))))
;;                     (post "/"          user-handler))))

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
