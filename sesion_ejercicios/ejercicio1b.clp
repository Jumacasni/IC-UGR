; Alumno: Juan Manuel Castillo Nievas

; Variable global para contar los hechos
(defglobal ?*n* = 0) 

; Inicialmente los hechos 
(deffacts ContadorInicial
        (NumeroHechos XXX 0)
        (ContarHechos)
)

; Primera regla: Si hay hechos (XXX ... ), se hace una assert para
;               que se cuenten los hechos que hay en ese momento
(defrule EmpezarContador
        ?n <- (XXX $?)
        =>
        (assert (ContarHechos))
        (bind ?*n* 0)
)

; Se cuentan los hechos del tipo (XXX ? ... ?) sumando 1 al contador global
(defrule Contador
        (declare (salience 100))

        (ContarHechos)

        (XXX $?)

        =>
        
        (bind ?*n* (+ ?*n* 1))
)

; Regla para eliminar hechos, pues el (retract ?n) no sirve
(defrule EliminarHechos
        
        ?f <- (EliminarHechos $?n)

        =>

        (retract ?f)

        (foreach ?field (create$ $?n)
                (retract ?field)
        )
        
        (bind ?*n* 0)
        (assert (ContarHechos))
)

; Modificar el número de hechos
(defrule Resultado
        (declare (salience 10))
        
        ?n <- (ContarHechos)
        ?modificar <- (NumeroHechos XXX ?)
        =>
        (retract ?n)
        (retract ?modificar)
        (assert (NumeroHechos XXX ?*n*))
        (bind ?*n* 0)
)
