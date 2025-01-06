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
     (def ptr
       (list-copy active-segments))
     (def (pop-ptr)
       (let ((elem (car ptr)))
         (set! ptr (cdr ptr))
         elem))
     (let ((var (conv (pop-ptr))) ...)
       (let ((body (bconv body-data)))
         statements)
       statements* ...))))

;;(define add-two
;;  (handler ((x :>number) (y :>number)) <- (_ :>)
;;           (displayln x)
;;           (displayln y)))
;;           (+ x y)))
