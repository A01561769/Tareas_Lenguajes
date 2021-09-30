#lang racket
;SECCION 1 
;Funcion que regresa la matriz dada sin la columna especificada por su posición en la matriz
; n = numero de columna a eliminar
; matrx = matriz a la que se le eliminara la columna
(define (elimina-columna n matrx)
  (if (null? matrx)
      '()
      (append (list (elimina n (car matrx))) (elimina-columna n (cdr matrx)))
      )
  )

;Funcion que regresa un renglón sin el elemento especificada por su posición
; n = numero de columna a eliminar
; reng = renglon al que se le eliminara el elemento
(define (elimina n reng)
  (cond
    ((null? reng) '())
    ((= n 1) (cdr reng))
    (else (append (list (car reng)) (elimina (- n 1) (cdr reng))))
      )
  )

(elimina-columna 3 '((4 0 3 1)(5 1 2 1)(6 0 1 1)))
;(elimina 3 '(4 0 3 1))
