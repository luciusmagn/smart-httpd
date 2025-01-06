;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/net/httpd
        :std/misc/uuid)
(export #t)

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
     (let ((var (conv (pop-ptr))) ...)
       (let ((body (bconv body-data)))
         statements)
       statements* ...))))

;;(define add-two
;;  (handler ((x :>number) (y :>number)) <- (_ :>)
;;           (displayln x)
;;           (displayln y)))
;;           (+ x y)))
