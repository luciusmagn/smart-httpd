;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/net/httpd
        :std/misc/uuid
        :std/misc/func
        :std/format
        :std/srfi/9
        :std/srfi/1)
(export #t)

(define-record-type <rejection>
  (rejection type msg)
  rejection?
  (type      rejection-type)
  (msg       rejection-msg))

;; handler conversions
;; :> is identity, because we always need something
(define (:> x)         x)
(define  :>string      :>) ; yeah
(define  :>number      string->number)
(define  :>keyword     string->keyword)
(define  :>symbol      string->symbol)
(define  :>uuid        string->uuid)

(defrules handler (<-)
  ((handler ((var conv) ...) <- (body bconv) statements statements* ...)
   (lambda (active-segments body-data)
     ;; just in case copy the list, I don't trust myself at this hour
     (def ptr
       (list-copy active-segments))

     ;; hack to let us not have to track the position in the list
     (def (pop-ptr)
       (let ((elem (car ptr)))
         (set! ptr (cdr ptr))
         elem))

     ;; actual implementation
     (call/cc
       (lambda (reject)
         (define (validate converted)
           (cond
            ;; TODO: decide if we want to make a nicer rejection
            ((boolean?   converted) (when (not converted) (reject converted)))
            ((rejection? converted) (reject converted))
            (else converted)))

         ;; catch possible exceptions
         ;; most-likely missing arguments
         (try
          (let ((var (validate (conv (pop-ptr)))) ...)
            (let ((body (bconv body-data)))
              statements
              statements* ...))
          (catch (e) (reject (rejection 'exception "An exception was caught")))))))))

(define add-two
  (handler ((x :>number) (y :>number)) <- (_ :>)
           (displayln x)
           (displayln y)
           (+ x y)))

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

;; (extract-params "/add/:x/:y" "/add/2")      => rejection
;; (extract-params "/add/:x/:y" "/add/2/3")    => ("2" "3")

(define-record-type <route-tree>
  (route-tree   segment-type children handler)
  route-tree?
  (segment-type route-tree-segment-type)
  (children     route-tree-children)
  (handler      route-tree-handler))

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

  (let* ((segments (string-split path #\/))
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
  (define (insert-route tree segments handler)
    (if (null? segments)
      (route-tree
       (route-tree-segment-type tree)
       (route-tree-children tree)
       handler)

      (let* ((seg            (car segments))
             (matching-child (find (lambda (child)
                                     (equal? (route-tree-segment-type child)
                                             (segment-type seg)))
                                   (route-tree-children tree))))
        (route-tree
         (route-tree-segment-type tree)
         (cons
          (if matching-child
            (insert-route matching-child
                          (cdr segments)
                          handler)
            (make-leaf seg
                       (cdr segments)
                       handler))
          (remv matching-child
                (route-tree-children tree)))
         (route-tree-handler tree)))))

  (define (make-leaf segment rest handler)
    (if (null? rest)
      (route-tree (segment-type segment) '() handler)
      (route-tree (segment-type segment)
                  (list (make-leaf (car rest)
                                   (cdr rest)
                                   handler))
                  #f)))

  (foldl (lambda (route tree)
           (insert-route tree
                         (car route)
                         (cadr route)))
         (route-tree #f '() #f)
         routes))

(define (print-route-tree routes)
  (define (padding   level)
    (list->string (repeat #\space (* level 2))))

  (define (tree-iter node level)
    (printf "~atype: ~a\n"
            (padding level)
            (route-tree-segment-type node))
    (for-each
      (lambda (n) (tree-iter n (+ 1 level)))
      (route-tree-children node)))

  (displayln "fraggot")
  (tree-iter routes 0))

(define test-treee (build-route-tree
                    `(((,(segment-exact "users")
                        ,(segment-dynamic "id"))
                       ,user-handler)
                      ((,(segment-exact "posts"))
                       ,posts-handler))))


(define (user-handler) (void))
(define (posts-handler) (void))

(define (handle-routing req res)
  ;; TODO
  (void))
