;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/net/httpd
        :std/misc/uuid
        :std/srfi/9)
(export #t)

(define-record-type <rejection>
  (rejection type msg)
  rejection?
  (type rejection-type)
  (msg rejection-msg))

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

     ;; hack to let us not have to track the thing thang
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

(define (handle-routing req res)
  ;; TODO
  (void))
