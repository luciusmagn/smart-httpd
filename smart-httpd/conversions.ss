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
        ./rejection
        ./cookie)
(export #t)

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

(define-headers-extractor
  :>cookies
  (lambda (headers)
    (let ((str (assget "Cookie" headers)))
      (if (string? st)
        (parse-cookies str)
        (parse-cookies "")))))

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

;;(define table-example
;;  (handler () <- (body :>json)
;;           (let ((name (hash-ref body 'name))
;;                 (age  (hash-ref body 'age))))))
;;             (string-append "Hello " name ", you are " (number->string age)))))
