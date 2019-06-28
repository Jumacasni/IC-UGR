; Alumno: Juan Manuel Castillo Nievas

; Abrir fichero
(defrule openfile 
        (declare (salience 30)) 
        => 
        (open "DatosT.txt" mydata)
        (assert (SeguirLeyendo))
) 

; Leer líneas del fichero y añadir hechos correspondientes
(defrule LeerValoresCierreFromFile
        (declare (salience 20)) 
        ?f <- (SeguirLeyendo)
        => 
        (bind ?Leido (readline mydata))
        (retract ?f)
        (if (neq ?Leido EOF) then
            (assert (T (explode$ ?Leido)))
            (assert (SeguirLeyendo))
        )
) 

; Cerrar fichero
(defrule closefile 
        (declare (salience 10)) 
        => 
        (close mydata) 
)