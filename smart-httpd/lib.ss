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
        ./conversions
        ./rejection
        ./cookie
        ./response
        ./router
        ./handler
        ./server)

(export (import: ./rejection))
(export (import: ./conversions))
(export (import: ./response))
(export (import: ./router))
(export (import: ./cookie))
(export (import: ./handler))
(export (import: ./server))
(export #t)


;;(define add-two
;;  (handler ((x :>number) (y :>number)) <- (_ :>)
;;           (displayln x)
;;           (displayln y)
;;           (+ x y)))


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


