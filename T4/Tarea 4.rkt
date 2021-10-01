#lang racket
; A00820039 Mariano Hurtado de Mendoza Carranza
; A00819647 Carlos Eduardo Govea Gonzales
; A01561769 Antonio Torres Carvajal

; ---------------------------------------- SECCION 1 - 1.a ----------------------------------------
; Funcion que regresa la matriz dada sin la columna especificada por su posición en la matriz
; n = numero de columna a eliminar
; matrx = matriz a la que se le eliminara la columna
(define (elimina-columna n matrx)
  (if (null? matrx)
      '()
      (append (list (elimina n (car matrx))) (elimina-columna n (cdr matrx)))
      )
  )

(define (elimina n reng)
  (cond
    ((null? reng) '())
    ((= n 1) (cdr reng))
    (else (append (list (car reng)) (elimina (- n 1) (cdr reng))))
      )
  )

; Casos de prueba
(elimina-columna 3 '((4 0 3 1)(5 1 2 1)(6 0 1 1)))

; ---------------------------------------- SECCION 1 - 1.b ----------------------------------------
; Funcion que agrega un nuevo valor a una matriz dada en la posición (renglón, columna) especificada, de la siguiente forma:
; si la matriz ya tiene un valor en esa posición, lo sustituye por el valor a agregar, pero si la matriz no tiene el número
; de renglones y/o columnas necesarias para colocar el nuevo valor, agregua los renglones y/o columnas mínimas necesarias
; para agregar el valor.
; val = numero a agregar
; pos = lista con la posición a agregar o cambiar (renglon, columna)
; matrx = matriz a modificar

(define (agrega-valor val pos matrx)
  (cond  ;Verificar si hay suf. rengs  Verificar si hay suf. cols
    ((or (> (car pos) (length matrx)) (> (cadr pos) (length (car matrx))))
     (cambia-valor val (car pos) (cadr pos)
                   (append (extender-existentes (- (cadr pos) (length (car matrx))) matrx)
                           (agrega-faltantes (if (> (cadr pos) (length (car matrx)))
                                                 (cadr pos)
                                                 (length (car matrx)))
                                             (- (car pos) (length matrx)))))
     )
     (else (cambia-valor val (car pos) (cadr pos) matrx))
    )
  )

; Funcion auxiliar que encuentra el renglon al que se le va a cambiar el valor
; val = numero a agregar
; reng = numero de renglon destino
; col = numero de columna destino
; matrx = matriz a modificar
(define (cambia-valor val reng col matrx)
  (if (> reng 1)
      (append (list (car matrx)) (cambia-valor val (- reng 1) col (cdr matrx)))
      (append (list (cambia-valor-Aux val col (car matrx))) (cdr matrx))
      )
  )

; Funcion auxiliar que encuentra la columna a la que se le va a cambiar el valor y lo cambia
; val = numero a agregar
; col = numero de columna destino
; renglon = renglon a modificar
(define (cambia-valor-Aux val col renglon)
  (if (> col 1)
      (append (list (car renglon)) (cambia-valor-Aux val (- col 1) (cdr renglon)))
      (append (list val) (cdr renglon))
      )
  )

; Funcion auxiliar que agrega las columnas faltantes en la matriz
; faltantes = columnas de 0s faltantes
; matrx = matriz base a extender
(define (extender-existentes faltantes matrx)
  (if (null? matrx)
      '()
      (append (list (append (car matrx) (crear-renglon faltantes))) (extender-existentes faltantes (cdr matrx)))
      )
  )

; Funcion auxiliar que agrega los renglones faltantes en la matriz
; tam = tamaño de la cada lista de 0s que falta
; cantidad = cantidad de renglones que faltan
(define (agrega-faltantes tam cantidad)
  (if (<= cantidad 0) '()
      (append (list (crear-renglon tam)) (agrega-faltantes tam (- cantidad 1)))
      )
  )

; Funcion que regresa una lista con cierta cantidad de 0s
; tam = tamaño la lista a crear
(define (crear-renglon tam)
  (if (= tam 0)
      '()
      (append '(0) (crear-renglon (- tam 1)))
      )
  )

; Casos de prueba
(agrega-valor 5 '(1 2) '((1 2 3)(0 2 1))) 
(agrega-valor 4 '(3 5) '((1 2 3)(0 2 1)))

; ---------------------------------------- SECCION 1 - 2.a ----------------------------------------
; Funcion que, dado un árbol binario de búsqueda regresa el rango de los valores contenidos en él
; como la lista (min max) donde min el valor más pequeño y max es el valor más grande.
;  = Arbol a analizar


; ---------------------------------------- SECCION 1 - 2.b ----------------------------------------
; Funcion que, regresa la cantidad de nodos en cierto nivel de un árbol binario.
;  = Arbol a analizar


; ---------------------------------------- SECCION 2 - 3.a ----------------------------------------
; Funcion que que lista los nodos destino que tienen a N como nodo origen directo.


; ---------------------------------------- SECCION 2 - 3.b ----------------------------------------
; Funcion que recibe como argumentos un grafo y el nombre de un nodo, y regresa el grafo sin el nodo
; especificado (si existe). También elimina los arcos dirigidos al nodo eliminado.

