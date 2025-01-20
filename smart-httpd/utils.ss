;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/misc/func
        :lho/fxns/lib)
(export #t)

(fn :ret flatten-rec ((lst : list?) -> list?)
    (if (null? lst)
      '()
      (let ((first (car lst))
            (rest (cdr lst)))
        (if (list? first)
          (append (flatten-rec first) (flatten-rec rest))
          (cons first (flatten-rec rest))))))
