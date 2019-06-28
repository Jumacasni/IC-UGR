; Alumno: Juan Manuel Castillo Nievas

; Variable global para contar los hechos
(defglobal ?*n* = 0) 

; Primera regla:
;       - Eliminar el hecho (ContarHechos XXX)
;       - Añadir (ContarHechos) a la base de hechos para empezar a contar
(defrule PrimeraRegla
        
        ?n <- (ContarHechos XXX)

        =>
        
        (assert (ContarHechos))
        (retract ?n)
)

; Cuando en la base de hechos hay (ContarHechos),
; se cuentan los hechos del tipo (XXX ? ... ?) sumando 1 al contador global
(defrule Contador
        (declare (salience 999))
        (ContarHechos)

        (XXX $?)

        =>
        
        (bind ?*n* (+ ?*n* 1))
)

; Una vez se han contado todos los hechos, si todavía no hay ningún hecho
; (NumeroHechos XXX ?n), significa que es la primera vez que se va a insertar
; un hecho de este tipo, con lo cual no hay que eliminar ningún hecho anterior
; El contador global se resetea a 0
(defrule ResultadoPrimero
        (declare (salience 10))
        
        (not (NumeroHechos XXX ?))

        ?eliminar1 <- (ContarHechos)        

        =>
        
        (assert (NumeroHechos XXX ?*n*))
        (retract ?eliminar1)
        (bind ?*n* 0)
)

; Si hay algún hecho del tipo (NumeroHechos XXX ?n), se elimina
; y se añade un nuevo hecho con el contador correcto
; El contador global se resetea a 0
(defrule Resultado
        (declare (salience 10))
        
        ?eliminar1 <- (NumeroHechos XXX ?)
        
        ?eliminar2 <- (ContarHechos)        

        =>
        
        (assert (NumeroHechos XXX ?*n*))
        (retract ?eliminar1)
        (retract ?eliminar2)
        (bind ?*n* 0)
)

