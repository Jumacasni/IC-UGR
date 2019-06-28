; Alumno: Juan Manuel Castillo Nievas

(deffacts Inicio
        (T 2 4 3 1 5)
)

; Abrir fichero
(defrule openfile 
        (declare (salience 30)) 
        => 
        (open "DatosT.txt" mydata "w") 
) 

; Escribir números en el fichero ordenados de menor a mayor
(defrule WriteData
        (declare (salience 20)) 
        (T $?cadena)
        => 
        (bind ?ordenados (implode$ (sort > $?cadena)))
        (printout mydata ?ordenados)
        (printout mydata crlf)
) 

; Cerrar fichero
(defrule closefile 
        (declare (salience 10)) 
        => 
        (close mydata) 
)