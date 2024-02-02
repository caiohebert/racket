#lang racket
; 1 - concatenar duas listas
(define (concatenar1 l1 l2)
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [else (cons (first l1) (concatenar1 (rest l1) l2))]
    )
  )

; 2 - concatenar duas listas, sendo que a segunda lista vem na frente.
(define (concatenarInv l1 l2)
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [else (cons (first l2) (concatenar1 (rest l2) l1))]
    )
  )

; 3 - concatenar uma lista de listas.
(define (concatenar2 l1)
  (cond
    [(null? (rest l1)) (first l1)]
    [else (concatenar1 (first l1) (concatenar2 (rest l1)))]
    )
  )

; 3-v - concatenar N listas.
(define (concatenar3 . listas)
  (cond
    [(null? listas) '()]
    [else (concatenar1 (first listas) (apply concatenar3 (rest listas)))]
    )
  )

; 4 - juntar duas listas, intercalando seus elementos.
(define (juntar l1 l2)
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [else (cons (first l1) (juntar l2 (rest l1)))]
    )
  )

; 5 - adicionar um elemento ao final de uma lista.
(define (adicionarFinal e l)
  (if (null? l)
      '(e)
      (concatenar1 l (cons e '()))
      )
  )

;6 - inverter uma lista.
(define (inverter l)
  (if (eq? (rest l) null)
      (cons (first l) '())
      (adicionarFinal (first l) (inverter (rest l)))
      )
  )

;7 - criar uma lista intercalada de tamanho N, de dois elementos e1 e e2.
(define (intercala n e1 e2)
  (if (< n 1)
      '()
      (cons e1 (intercala (- n 1) e2 e1))
      )
   )

; 8 - criar uma lista intercalada de tamanho N, de M elementos eM.
(define (intercala2 n . elementos)
  (cond
    [(< n 1) '()]
    [else (cons (first elementos) (apply intercala2 (- n 1) (adicionarFinal (first elementos) (rest elementos))))]
    )
  )

; 9 - recebe um elemento E e uma lista L, e produz a lista de pares cujo primeiro elemento
;     é E e o segundo elemento é um membro de L.
(define (parear e l)
  (if (null? l)
      '()
      (cons (cons e (cons (first l) '())) (parear e (rest l)))
      )
  )

; 10 - recebe uma lista L e produz a lista de todos os pares de elementos de L.
(define (pares l)
  (if (null? l)
      '()
      (concatenar1 (parear (first l) (rest l)) (pares (rest l)))
      )
  )

;12 - recebe uma lista e testa se ela tem elementos repetidos (pode ser interpretada como um
;     conjunto).
(define (member? e l)
  (cond
    [(null? l) #f]
    [(eq? e (first l)) #t]
    [else (member? e (rest l))]
    )
  )

(define (conjunto? l)
  (cond
    [(null? l) #t]
    [(member? (first l) (rest l)) #f]
    [else (conjunto? (rest l))]
    )
  )

;13 - recebe duas listas e testa se a primeira é prefixo da segunda.
(define (prefixo? l1 l2)
  (cond
    [(null? l1) #t]
    [(and (null? l2) (not (null? l1))) #f]
    [(eq? (first l1) (first l2)) (prefixo? (rest l1) (rest l2))]
    [else #f]
    )
  )

; 14 - recebe duas listas e testa se a primeira é subsequência da segunda.
(define (subsequencia? l1 l2)
  (cond
    [(null? l2) #f]
    [(eq? (first l1) (first l2)) (prefixo? l1 l2)]
    [else (subsequencia? l1 (rest l2))]
    )
  )

; 15 - usando equal?, recebe duas listas genérica e testa se elas são iguais.
(define (iguais-lg? lg1 lg2)
  (cond
    [(and (null? lg1) (null? lg2)) #t]
    [(and (not (or (list? (first lg1)) (list? (first lg2)))) (eq? (first lg1) (first lg2))) (iguais-lg? (rest lg1) (rest lg2))]
    [(and (list? (first lg1)) (list? (first lg2))) (iguais-lg? (concatenar2 lg1) (concatenar2 lg2))]
    [else #f]
    )
  )