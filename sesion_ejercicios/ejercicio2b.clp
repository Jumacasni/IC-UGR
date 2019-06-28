; Alumno: Juan Manuel Castillo Nievas

(deffacts Inicio
        (T 31 5 4 6 7 10)
)

(deffunction minXiT ()
    
    ; Inicialmente el mínimo no existe
    (bind ?min null)

    (do-for-fact ((?f T)) TRUE
        
        ; Recorrer cada número y encontrar el mínimo
        (foreach ?field (create$ (fact-slot-value ?f implied))
            (if (eq ?min null) then     
                (bind ?min ?field)

            else
                (if (< ?field ?min) then     
                    (bind ?min ?field)
                ) 
            )
        )
    )

    ; Devolver el mínimo
    ?min
)

