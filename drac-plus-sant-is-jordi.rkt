#lang racket

; By Jacob J. A. Koot, 2020

(require "../fmt/fmt.rkt") ; Nothing provided.

;----------------------------------------------------------------------------------------------------

(define header
 "
 ┌────────────────────────────────────┐
 │ Sant Jordi, día 23 de febrero 2020 │
 └────────────────────────────────────┘

 Todas las permutaciones  (A C D I J N O R S T)
 de los dígitos decimales (0 1 2 3 4 5 6 7 8 9) tal que:

       DRAC
       SANT
    + ─────
      JORDI

 Permutaciones con J=0 incluidas o excluidas.
 Tenga en cuenta que J=0 o J=1, nunca J>1. \n\n")

;----------------------------------------------------------------------------------------------------

(define (make-list-of-solutions) ; J=0 included.
 (sort
  (for*/list ; nr of iterations: 10!/3! = 604800, the ones excluded by the when-clause included.
   ((combination (in-combinations '(0 1 2 3 4 5 6 7 8 9) 7))
    (permutation (in-permutations combination))
    ((A C D N R S T) (in-values (apply values permutation)))
    (DRAC  (in-value (+ (* 1000 D) (* 100 R) (* 10 A) C)))
    (SANT  (in-value (+ (* 1000 S) (* 100 A) (* 10 N) T)))
    (JORDI (in-value (+ DRAC SANT)))
    (J (in-value (find-digit JORDI 10000)))
    (O (in-value (find-digit JORDI 1000)))
    (I (in-value (find-digit JORDI 1)))
    #:when
    (and
     (= (find-digit JORDI 10) D)
     (= (find-digit JORDI 100) R)
     (not (check-duplicates (list J O I) =))
     (not (member J combination =))
     (not (member O combination =))
     (not (member I combination =))))
   (list DRAC SANT JORDI (list A C D I J N O R S T)))
  solution<?))

;----------------------------------------------------------------------------------------------------

(define (display-solutions J=0-included?)

 (define numbered-solutions
  (for/list
   ((n (in-naturals 1))
    (solution (in-list solutions))
    #:when (or J=0-included? (J=1? solution)))
    (cons n solution)))

 (define JORDIS (map fourth numbered-solutions))

 (fmt0 J=0-included?)

 (if J=0-included?
  (fmt1
   (length numbered-solutions)
   (count (λ (JORDI) (> JORDI  9999)) JORDIS)
   (count (λ (JORDI) (< JORDI 10000)) JORDIS))
  (fmt2 (length numbered-solutions)))

 (fmt-3-4-5a numbered-solutions)

 (for ((i 0-9))
  (let ((lst (for/list ((j 0-9)) (count (λ (x) (= (list-ref (digits x) j) i)) numbered-solutions))))
   (fmt5b i (map erase-zero lst))))

 (fmt-5c-6 (length numbered-solutions)))

;----------------------------------------------------------------------------------------------------

(define line0a (fmt "' ┌──────────────────────────────┐\n'"))
(define line0b (fmt "' └──────────────────────────────┘\n'"))
(define line3a (fmt "'    ┌────┬─────────────────────┬─────────────────────┐\n'"))
(define line3b (fmt "'    ├────┼─────────────────────┼─────────────────────┤\n'"))
(define line3c (fmt "'    └────┴─────────────────────┴─────────────────────┘\n'"))
(define line5a (fmt "'    ┌───────┬───────────────────────────────┐\n'"))
(define line5b (fmt "'    ├───────┼───────────────────────────────┤\n'"))
(define line5c (fmt "'    └───────┴───────────────────────────────┘\n'"))
(define fmt0 (fmt 'cur line0a "' │ Soluciones con J=0 ' Q'incluidas │' 'excluidas │' S/" line0b "/"))
(define fmt1 (fmt 'cur "' Hay 'I2' soluciones, 'I2' con J=1 y 'I2' con J=0:'/"))
(define fmt2 (fmt 'cur "' Hay ' I2 ' soluciones:'/"))
(define fmt5a (fmt 'cur line5a "'    │ valor │  A  C  D  I  J  N  O  R  S  T │'/" line5b))
(define fmt5b (fmt 'cur "'    │ 'I5' │'US10D' │'/"))
(define fmt5c (fmt 'cur line5c "/"))
(define digits fifth)
(define jordi caddr)
(define sant cadr)
(define fmtXI2 (fmt "XI2"))
(define (erase-zero x) (if (zero? x) "   " (fmtXI2 x)))
(define 0-9 (in-range 0 10))
(define (find-digit n significance) (modulo (quotient n significance) 10))
(define (J=1? solution) (>= (third solution) 10000))

(define fmt3
 (fmt 'cur
  "/" line3a
  "'    │ nr │ DRAC + SANT = JORDI │ A C D I J N O R S T │'/"
  line3b
  "U#('    │ 'USI2' │ 'I4.4' + 'I4.4' = 'I5.5' │'U#I2' │'/)"
  line3c "/"))

(define fmt4
 (fmt 'cur
  "' Número de soluciones en las el dígito'/
   ' A, C, D, I, J, N, O, R, S o T tiene el valor'/
   ' 0, 1, 2, 3, 4, 5, 6, 7, 8 o 9:'//"))

(define fmt6
 (fmt 'cur
  "' Cada columna y cada fila suma a 'D','/"
  "' es decir, al número total de soluciones.'/"
  "' Esto está evidente para las columnas.'/"
  "' También se aplica a las filas porque cada solución'/"
  "' contiene cada dígito exactamente una vez.'//"))

(define fmt-3-4-5a (fmt 'cur fmt3 fmt4 fmt5a))
(define fmt-5c-6 (fmt 'cur fmt5c fmt6))

(define (solution<? x y)
 (let ((xj (jordi x)) (yj (jordi y)))
  (or (< xj yj) (and (= xj yj) (< (sant x) (sant y))))))

(define-sequence-syntax in-values
 (λ (stx) (raise-syntax-error 'in-values "can be used in for forms only" stx))
 (λ (stx)
  (syntax-case stx ()
   (((id ...) (_ expr))
  #'((id ...) (:do-in (((id ...) expr)) #t () #t () #t #f ()))))))

;----------------------------------------------------------------------------------------------------

(define solutions (make-list-of-solutions))
(display header)
(display-solutions #t)
(display-solutions #f)

;----------------------------------------------------------------------------------------------------

((fmt 'cur
  "' ┌────────┐'/"
  "' │ El fin │'/"
  "' └────────┘'//"))
