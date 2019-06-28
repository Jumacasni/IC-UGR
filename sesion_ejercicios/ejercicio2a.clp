; Alumno: Juan Manuel Castillo Nievas

(deftemplate T
        (slot dato)
        (slot S)
)

(deffacts Inicio
        (T (dato 1) (S 4))
        (T (dato 2) (S 10))
        (T (dato 3) (S 5))
        (T (dato 4) (S 6))
        (T (dato 5) (S 3))
        (T (dato 6) (S 14))
        (T (dato 7) (S 12))
)

(deffunction minSdeT ()
    
    ; Inicialmente el mínimo no existe
    (bind ?min null)

    ; Recorrer los números y encontrar el mínimo
    (do-for-all-facts ((?template T)) TRUE
        (if (eq ?min null) then     
            (bind ?min ?template:S)

        else
            (if (< ?template:S ?min) then     
                (bind ?min ?template:S)
            ) 
        )
    )

    ; Devolver mínimo
    ?min
)