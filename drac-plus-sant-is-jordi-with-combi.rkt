#lang racket

;----------------------------------------------------------------------------------------------------

(display
"
 -----------------------------
 Sant Jordi, día 23 de febrero
 -----------------------------

 Todas las permutaciones  (A C D I J N Ø R S T)
 de los dígitos decimales (0 1 2 3 4 5 6 7 8 9) tal que:

       DRAC
       SANT
    + -----
      JØRDI

 Permutaciones con J=0 incluidas o excluidas.
 Tenga en cuenta que J=0 o J=1, nunca J>1.

 ‘Ø’ es la letra mayúscula decimoquinta del alfabeto latino.
 ‘0’ es el dígito cero.\n\n")

;----------------------------------------------------------------------------------------------------

(require "fmt/fmt.rkt") ; Nothing provided.
(define (fmt-cur . fmts) (apply fmt 'cur fmts))
(define 0-9 (in-range 0 10))
(define significances #(1 10 100 1000 10000))
(define (find-decimal-digit n i) (modulo (quotient n (vector-ref significances i)) 10))
(define (J=1? solution) (>= (caddr solution) 10000))

;----------------------------------------------------------------------------------------------------
; solutions = ((DRAC SANT JØRDI (A C D I J N Ø R S T)) ...)
; In each solution
; (A C D I J N Ø R S T) is a permutation of
; (0 1 2 3 4 5 6 7 8 9) and
; DRAC + SANT = JØRDI.
;----------------------------------------------------------------------------------------------------

(define-sequence-syntax in-values
 (λ (stx) (raise-syntax-error 'in-values "can only be used in for forms" stx))
 (λ (stx)
  (syntax-case stx ()
   (((id ...) (_ expr))
  #'((id ...) (:do-in (((id ...) expr)) #t () #t () #t #f ()))))))

(define (digits->number . digits)
 (for/fold ((number 0)) ((digit (in-list digits))) (+ (* 10 number) digit)))

(define solutions ; J=0 included.
 (sort
  (for*/list
   ((combination (in-combinations '(0 1 2 3 4 5 6 7 8 9) 7))
    (permutation (in-permutations combination))
    ((A C D N R S T) (in-values (apply values permutation)))
    (DRAC  (in-value (digits->number D R A C)))
    (SANT  (in-value (digits->number S A N T)))
    (JØRDI (in-value (+ DRAC SANT)))
    (J (in-value (find-decimal-digit JØRDI 4)))
    (Ø (in-value (find-decimal-digit JØRDI 3)))
    (I (in-value (find-decimal-digit JØRDI 0)))
    #:when
    (and
     (not (check-duplicates (list J Ø I) =))
     (not (member J combination =))
     (not (member Ø combination =))
     (not (member I combination =))
     (= (find-decimal-digit JØRDI 1) D) 
     (= (find-decimal-digit JØRDI 2) R)))
   (list DRAC SANT JØRDI (list A C D I J N Ø R S T)))
  (λ (x y)
   (or (< (caddr x) (caddr y))
    (and (= (caddr x) (caddr y))
         (< (cadr x) (cadr y)))))))

;----------------------------------------------------------------------------------------------------

(define (display-solutions J=0-included?)
 (let ((solutions (if J=0-included? solutions (filter J=1? solutions))))
 
  ((fmt-cur "' ----------------------------'/
             ' Soluciones con J=0 ' Q'incluidas' 'excluidas' S/
             ' ----------------------------'//") J=0-included?) 
 
  (define JØRDIS (map caddr solutions))
 
  (if J=0-included?
   ((fmt-cur "' Hay ' I2 ' soluciones, ' I2 ' con J=1 y ' I2 ' con J=0.'/")
    (length solutions)
    (count (λ (n) (> n  9999)) JØRDIS)
    (count (λ (n) (< n 10000)) JØRDIS))
   ((fmt-cur "' Hay ' I2 ' soluciones.'/") (length solutions)))
 
  ((fmt-cur "/ X 'DRAC + SANT = JØRDI' 3X 'A C D I J N Ø R S T' //
             U#(XUS I4.4 ' + ' I4.4 ' = ' I5.5 2X U#I2/)/")
   solutions)
 
  ((fmt-cur "' Valores que asumen los dígitos:'//"))
 
  (for ((i 0-9))
       (let ((letter (string-ref "ACDIJNØRST" i))
             (digits (sort (remove-duplicates (map (λ (x) (list-ref (cadddr x) i)) solutions) =) <)))
            ((fmt-cur "XD' :'U#I2/") letter digits)))
 
  (newline)
 
  ((fmt-cur "' Número de soluciones en las el dígito'/
             ' A, C, D, I, J, N, Ø, R, S o T tiene el valor'/
             ' 0, 1, 2, 3, 4, 5, 6, 7, 8 o 9.'//"))
 
  (define line (fmt "' |-------|-------------------------------|'/"))
 
  ((fmt-cur line "' | valor |  A  C  D  I  J  N  Ø  R  S  T |'/" line))
 
  (define (erase-zero x) (if (zero? x) "   " ((fmt "I3") x)))
 
  (let ((f (fmt-cur "' | ' I5 ' |' US10D ' |' /")))
   (for ((i 0-9))
    (let ((lst (for/list ((j 0-9)) (count (λ (x) (= (list-ref (cadddr x) j) i)) solutions))))
     (f i (map erase-zero lst)))))
 
  ((fmt-cur line
    "/"
    "' Cada columna y cada fila suma a ' D ','/"
    "' es decir, al número total de soluciones.'/"
    "' Esto está evidente para las columnas.'/"
    "' También se aplica a las filas porque cada solución'/"
    "' contiene cada dígito exactamente una vez.'//")
   (length solutions))))

;----------------------------------------------------------------------------------------------------

(display-solutions #t)
(display-solutions #f)

;----------------------------------------------------------------------------------------------------

((fmt-cur "' El fin'//"))
