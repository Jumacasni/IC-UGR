; Alumno: Juan Manuel Castillo Nievas

(defglobal ?*temporizador* = 0)

; Añadir hecho (Contar) cuando ya no haya ninguna regla posible
(defrule Espera
        (declare (salience -10))

        =>
        (assert (Contar))
        (bind ?*temporizador* (time))
)

; Contar hasta 60 segundos y reiniciar contador
(defrule Contar
        ?f <- (Contar)

        =>
        (retract ?f)
        (bind ?t (time))
        (bind ?diferencia (- ?t ?*temporizador*))
        (if (> ?diferencia 60) then
                (assert (ReiniciarContador))

        else
                (assert (Contar))
        )
)

; Cuando se reinicie el contador, imprimir en pantalla que se está esperando
; nueva información
(defrule ImprimirInfo
        ?f <- (ReiniciarContador)

        =>
        (assert (Contar))
        (retract ?f)
        (bind ?*temporizador* (time))
        (printout t "Estoy esperando nueva informacion" crlf)
)