; /******************************************************/
; /                        TEMPLATES                     /
; /******************************************************/

; /*
; * CONOCIMIENTO SOBRE LA CASA:
; *     - Habitacion: nombre de la habitación y número de ventanas
; *     - Puerta: dos habitaciones contiguas con una puerta
; *     - Paso: dos habitaciones contiguas pero sin pared
; *     - Mínimo luz: el mínimo de luz que debe tener una habitación
; */

(deftemplate Habitacion
        (slot habitacion)
        (slot ventanas)
)

(deftemplate Puerta
        (slot puerta_habitacion1)
        (slot puerta_habitacion2)
)

(deftemplate Paso
        (slot paso_habitacion1)
        (slot paso_habitacion2)
)

(deftemplate minimo_luz
        (slot habitacion)
        (slot luz)
)

; /*
; * TEMPLATES DEL CONOCIMIENTO QUE ES DEDUCIDO POR LAS REGLAS POSTERIORES
; */

; /* Estado de una habitación (activa, parece_inactiva, inactiva) en un determinado instante
(deftemplate estado_habitacion
        (slot habitacion)
        (slot estado)
        (slot instante)
)

; /*
; * HECHOS INICIALES DEL CONOCIMIENTO DE LA CASA
; */

(deffacts Habitaciones
        (Habitacion
                (habitacion garaje)
                (ventanas 1)
        )

        (Habitacion
                (habitacion salon)
                (ventanas 2)
        )

        (Habitacion
                (habitacion comedor)
                (ventanas 2)
        )

        (Habitacion
                (habitacion cocina)
                (ventanas 1)
        )

        (Habitacion
                (habitacion dormitorio1)
                (ventanas 2)
        )

        (Habitacion
                (habitacion dormitorio2)
                (ventanas 2)
        )

        (Habitacion
                (habitacion dormitorio3)
                (ventanas 0)
        )

        (Habitacion
                (habitacion banio1)
                (ventanas 1)
        )

        (Habitacion
                (habitacion banio2)
                (ventanas 0)
        )

        (Habitacion
                (habitacion despensa)
                (ventanas 0)
        )

        (Habitacion
                (habitacion pasillo)
                (ventanas 0)
        )

        (Habitacion
                (habitacion entrada)
                (ventanas 2)
        )
)

(deffacts Puertas
        (Puerta
                (puerta_habitacion1 entrada)
                (puerta_habitacion2 salon)
        )

        (Puerta
                (puerta_habitacion1 cocina)
                (puerta_habitacion2 comedor)
        )

        (Puerta
                (puerta_habitacion1 pasillo)
                (puerta_habitacion2 cocina)
        )

        (Puerta
                (puerta_habitacion1 pasillo)
                (puerta_habitacion2 dormitorio1)
        )

        (Puerta
                (puerta_habitacion1 pasillo)
                (puerta_habitacion2 dormitorio2)
        )

        (Puerta
                (puerta_habitacion1 pasillo)
                (puerta_habitacion2 despensa)
        )

        (Puerta
                (puerta_habitacion1 pasillo)
                (puerta_habitacion2 banio2)
        )

        (Puerta
                (puerta_habitacion1 dormitorio3)
                (puerta_habitacion2 banio2)
        )

        (Puerta
                (puerta_habitacion1 despensa)
                (puerta_habitacion2 garaje)
        )

        (Puerta
                (puerta_habitacion1 dormitorio3)
                (puerta_habitacion2 salon)
        )
)

(deffacts Pasos
        (Paso
                (paso_habitacion1 dormitorio1)
                (paso_habitacion2 banio1)
        )

        (Paso
                (paso_habitacion1 salon)
                (paso_habitacion2 comedor)
        )
)

(deffacts MinimosLuz

    (minimo_luz
            (habitacion garaje)
            (luz 150)
    )

    (minimo_luz
            (habitacion salon)
            (luz 350)
    )

    (minimo_luz
            (habitacion comedor)
            (luz 350)
    )

    (minimo_luz
            (habitacion cocina)
            (luz 350)
    )

    (minimo_luz
            (habitacion dormitorio1)
            (luz 100)
    )

    (minimo_luz
            (habitacion dormitorio2)
            (luz 100)
    )

    (minimo_luz
            (habitacion dormitorio3)
            (luz 100)
    )

    (minimo_luz
            (habitacion banio1)
            (luz 150)
    )

    (minimo_luz
            (habitacion banio2)
            (luz 150)
    )

    (minimo_luz
            (habitacion despensa)
            (luz 150)
    )

    (minimo_luz
            (habitacion pasillo)
            (luz 300)
    )

    (minimo_luz
            (habitacion entrada)
            (luz 150)
    )
)

; /* Inicialmente todas las habitaciones tienen la ultima_desactivacion en el instante 0 (ejercicio 2)

(defrule IniciarUltimaDesactivacion
        (Habitacion (habitacion ?h))
        (not (ultima_desactivacion movimiento ?h ?instante))

        =>
        (assert (ultima_desactivacion movimiento ?h 0))
)

; /* Inicialmente todas las habitaciones tengan la ultima_activacion en el instante 0 (ejercicio 2)

(defrule IniciarUltimaActivacion
        (Habitacion (habitacion ?h))
        (not (ultima_activacion movimiento ?h ?instante))

        =>
        (assert (ultima_activacion movimiento ?h 0))
)

; /* Inicialmente todas las habitaciones están inactivas

(defrule EstadoHabitacionesInicial
    (Habitacion (habitacion ?h))
    (not (estado_habitacion (habitacion ?h) (estado ?estado) (instante ?ins)))

    =>

    (assert (estado_habitacion (habitacion ?h) (estado inactiva) (instante 0)))
)

; Función para obtener el máximo de dos valores
(deffunction maximo (?value1 ?value2)
  (> ?value1 ?value2)
)

; Función para obtener el mínimo de dos valores
(deffunction minimo (?value1 ?value2)
  (< ?value1 ?value2)
)

; /******************************************************/
; /                        REGLAS                        /
; /******************************************************/

; /******** EJERCICIO 1: Primeras deducciones ********/

; /*
; * Apartado a) Si se puede pasar directamente (por una puerta o paso)
; *             de una habitación a otra, añadir a la base de hechos
; *             (posible pasar habitacion1 habitacion2)
; */

; /* Si h1 y h2 tienen una puerta, es posible pasar desde h1 a h2 y viceversa
(defrule PosiblePasarPuertas
        (declare (salience 1000))
        (Habitacion
                (habitacion ?H1)
                (ventanas ?))

        (Habitacion
                (habitacion ?H2 & ~?H1)
                (ventanas ?))

        (Puerta
                (puerta_habitacion1 ?H1|?H2)
                (puerta_habitacion2 ?H2|?H1))
        =>
        (assert (posible_pasar ?H1 ?H2))
)

; /* Si hay un paso en h1 y h2, es posible pasar desde h1 a h2 y viceversa
(defrule PosiblePasarPasos
        (declare (salience 1000))
        (Habitacion
                (habitacion ?H1)
                (ventanas ?))

        (Habitacion
                (habitacion ?H2 & ~?H1)
                (ventanas ?))

        (Paso
                (paso_habitacion1 ?H1|?H2)
                (paso_habitacion2 ?H2|?H1))
        =>

        (assert (posible_pasar ?H1 ?H2))
)

; /*
; * Apartado b) Si para acceder a una habitación solo se puede pasar desde otra,
; *             añadir a la base de hechos
; *             (necesario_pasar habitacion1 habitacion2)
; */

; /* PASO 1
; * Encontrar las habitaciones que tienen más de una puerta (conectan con más
; * de una habitación)
; */

; /* Si una habitación h1 conecta con h2 y con otra h3,
; /* h1 tiene más de una puerta
(defrule HabitacionesConMasDeUnaPuerta
        (declare (salience 999))
        (Habitacion
                (habitacion ?H1)
                (ventanas ?))

        (Habitacion
                (habitacion ?H2 & ~?H1)
                (ventanas ?))

        (Habitacion
                (habitacion ?H3 & ~?H1 & ~ ?H2)
                (ventanas ?))

        (posible_pasar ?H1 ?H2)

        (posible_pasar ?H1 ?H3)

        =>
        (assert (habitacion_con_mas_de_una_puerta ?H1))
)

; /* PASO 2
; * Encontrar las habitaciones que sólo tienen una puerta (conectan con sólo
; * una habitación)
; */

(defrule HabitacionesConUnaSolaPuerta
        (declare (salience 998))
        (Habitacion
                (habitacion ?H1)
                (ventanas ?))

        (not (habitacion_con_mas_de_una_puerta ?H1))

        =>
        (assert (habitacion_con_una_sola_puerta ?H1))
)

; /* PASO 3
; * A partir de las habitaciones con una sola puerta, obtengo las parejas para
; * (necesario_pasar)
; */

(defrule NecesarioPasar
        (declare (salience 997))
        (habitacion_con_una_sola_puerta ?H1)

        (posible_pasar ?H1 ?H2)
        =>
        (assert (necesario_pasar ?H1 ?H2))
)

; /*
; * Apartado c) Si una habitación es interior, añadir el hecho
; *             (habitacion_interior habitacion)
; */


(defrule HabitacionInterior
        (Habitacion
                (habitacion ?H)
                (ventanas 0))
        =>
        (assert (habitacion_interior ?H))
)

; /******** EJERCICIO 2: Registro de los datos de los sensores ********/

; /*
; * Apartado a1) Registro de los datos proporcionados
; */

; /* Registro del sensor de movimiento cuando está en ON
; /* 1) Añado el valor valor_registrado
; /* 2) Añado último registro
; /* 3) Añado última activación

(defrule SensorMovimientoOn
        (declare (salience 911))
        ?n <- (valor movimiento ?habitacion on)
        =>
        (bind ?instante (totalsegundos ?*hora* ?*minutos* ?*segundos*))

        (assert (valor_registrado ?instante movimiento ?habitacion on))

        (assert (ultimo_registro movimiento ?habitacion ?instante))

        (assert (ultima_activacion movimiento ?habitacion ?instante))

        (retract ?n)
)

; /* Registro del sensor de movimiento cuando está en OFF
; /* 1) Añado el valor valor_registrado
; /* 2) Añado último registro
; /* 3) Añado última desactivación
(defrule SensorMovimientoOff
        (declare (salience 911))
        ?n <- (valor movimiento ?habitacion off)
        =>
        (bind ?instante (totalsegundos ?*hora* ?*minutos* ?*segundos*))

        (assert (valor_registrado ?instante movimiento ?habitacion off))

        (assert (ultimo_registro movimiento ?habitacion ?instante))

        (assert (ultima_desactivacion movimiento ?habitacion ?instante))

        (retract ?n)
)

; /* Registro del sensor de luminosidad
; /* 1) Añado el valor valor_registrado
; /* 2) Añado último registro
(defrule SensorLuminosidad
        (declare (salience 911))
        ?n <- (valor luminosidad ?habitacion ?l)
        =>
        (bind ?instante (totalsegundos ?*hora* ?*minutos* ?*segundos*))

        (assert (valor_registrado ?instante luminosidad ?habitacion ?l))

        (assert (ultimo_registro luminosidad ?habitacion ?instante))

        (retract ?n)
)

; /* Registro del sensor del pulsador
; /* 1) Añado el valor valor_registrado
; /* 2) Añado último registro
(defrule SensorPulsador
        (declare (salience 911))
        ?n <- (valor estadoluz ?habitacion ?estado)
        =>
        (bind ?instante (totalsegundos ?*hora* ?*minutos* ?*segundos*))

        (assert (valor_registrado ?instante estadoluz ?habitacion ?estado))

        (assert (ultimo_registro estadoluz ?habitacion ?instante))

        (retract ?n)
)

; /*
; * Apartado a2) Registrar último registro del sensor de una habitación
; */

; /* 1) De entre todos los hechos (valor_registrado ?instante ?m ?hab ?res)
; /*    se coge el valor registrado con el tiempo máximo
; /* 2) Se borran los hechos (ultimo_registro ?m ?hab ?i) que sean menores
; /*    que el tiempo máximo del paso 1)

(defrule UltimoRegistro
        (declare (salience 910))
        (valor_registrado ?instante ?m ?hab ?res)
        (not (valor_registrado ?value2&:(maximo ?value2 ?instante) ?m ?hab ?res))
        ?n <- (ultimo_registro ?m ?hab ?i)
        (test (< ?i ?instante))
        =>
        (retract ?n)
)

; /*
; * Apartado a3) Registrar última activación, última vez que el sensor de
; *             movimiento pasó de OFF a ON
; */

; /* PASO 1
; * 1) Se coge el momento en el que se produjo la última desactivación
; * 2) Se cogen los hechos (ultima_activacion) cuyo instante sea mayor que
; *     el momento del paso 1)
; * 3) De entre todos los hechos del paso 2), se coge el que tenga el mínimo tiempo
; * 4) Se borran los hechos del paso 2) que no sean mínimos
; */

(defrule UltimaActivacion1
        (declare (salience 909))
        (ultima_desactivacion movimiento ?hab ?ultima_desactivacion)

        (ultima_activacion movimiento ?hab ?instante)
        (test (> ?instante ?ultima_desactivacion))

        (not (ultima_activacion movimiento ?hab ?v2&:(minimo ?v2 ?instante)))
        ?borrar2 <- (ultima_activacion movimiento ?hab ?i)
        (test (> ?i ?instante))

        =>

        (retract ?borrar2)
)

; /* PASO 2
; * 1) Se coge el hecho (ultima_activacion) con el instante máximo
; * 2) Se borran los hechos del paso 1) que no tengan el instante máximo
; */

(defrule UltimaActivacion2
        (declare (salience 908))
        (ultima_activacion movimiento ?hab ?instante)
        (not (ultima_activacion movimiento ?hab ?value2&:(maximo ?value2 ?instante)))
        ?borrar1 <- (ultima_activacion movimiento ?hab ?i)
        (test (< ?i ?instante))

        =>

        (retract ?borrar1)
)

; /*
; * Apartado a3) Registrar última desactivación, última vez que el sensor de
; *             movimiento pasó de ON a OFF
; */

; /*
; * 1) Se coge el máximo instante de los hechos (valor_registrado movimiento off)
; * 2) Se borran los hechos (ultima_desactivacion) cuyo instante sea menor
; *     que el máximo del paso 1)
; */

(defrule UltimaDesactivacion
        (declare (salience 909))
        (valor_registrado ?instante movimiento ?hab off)
        (not (valor_registrado ?value2&:(maximo ?value2 ?instante) movimiento ?hab off))
        ?n <- (ultima_desactivacion movimiento ?hab ?i)
        (test (< ?i ?instante))
        =>
        (retract ?n)
)

; /*
; * Apartado b) Informe de datos recibidos
; */

(defrule Informe
        (informe ?h)

        (valor_registrado ?instante ?t ?h ?res)

        =>
        (bind ?horas (hora-segundos ?instante))
        (bind ?minutos (minuto-segundos ?instante))
        (bind ?segundos (segundo-segundos ?instante))
        (printout t "Sensor: " ?t "    Instante: " ?horas ":" ?minutos ":" ?segundos "    Resultado: " ?res crlf)

)

; /******** EJERCICIO 3: Manejo de luces con el conocimiento propio ********/

; /*
; * REGLA 1: Si una habitación está activa y hay poca luz, se enciende la luz
; */

; /* 1) Se miran las habitaciones activas
; /* 2) Se coge la luminosidad de la habitación en el último registro de luminosidad
; /* 3) Se comprueba si la luminosidad está por debajo de la mínima

(defrule HabitacionActivaPocaLuz
        (declare (salience 900))
        (HoraActualizada ?horaactual)

        (estado_habitacion (habitacion ?h) (estado activa) (instante ?ins))
        (ultimo_registro luminosidad ?h ?i)
        (valor_registrado ?i luminosidad ?h ?luminosidad)
        (minimo_luz (habitacion ?h) (luz ?l))
        (test (< ?luminosidad ?l))
        (test (eq ?horaactual ?ins)) ; Para que sólo mande la acción de encender luz una vez solamente
        =>
        (assert (accion pulsador_luz ?h encender))
)

; /*
; * REGLA 2: Si una habitación está vacía y la luz encendida, se apaga la luz
; */

; /* 1) Se miran las habitaciones inactivas
; /* 2) Se coge el valor registrado del último registro del estado de la luz
; /* 3) Se comprueba si el último valor registrado es ON

(defrule HabitacionVacia
        (declare (salience 900))
        (HoraActualizada ?horaactual)

        (estado_habitacion (habitacion ?h) (estado inactiva) (instante ?i))
        (ultimo_registro estadoluz ?h ?ins)
        (valor_registrado ?ins estadoluz ?h on)
        (test (eq ?horaactual ?i)) ; Para que sólo mande la acción de apagar luz una vez solamente
        =>
        (assert (accion pulsador_luz ?h apagar))
)

; /*
; * REGLA 3: Si la luz está en ON y hay mucha luminosidad, apagar luz
; */

; /* 1) Coger las habitaciones que tengan el pulsador en ON
; /* 2) De las habitaciones del paso 1), coger la última luminosidad registrada
; /* 3) Se comprueba si hay el doble de luminosidad de la mínima

(defrule MuchaLuminosidad
        (declare (salience 900))
        (HoraActualizada ?horaactual)

        (ultimo_registro estadoluz ?h ?instante_e)
        (valor_registrado ?instante_e estadoluz ?h on)

        (ultimo_registro luminosidad ?h ?instante_l)
        (valor_registrado ?instante_l luminosidad ?h ?luminosidad)

        (minimo_luz (habitacion ?h) (luz ?l))

        (test (> ?luminosidad (+ ?l ?l)))
        (test (eq ?horaactual ?instante_e)) ; Para que sólo mande la acción de apagar luz una vez solamente
        =>
        (assert (accion pulsador_luz ?h apagar))
)
; /*
; * REGLA 4: Si el sensor de movimiento de una habitación está en ON, habitación activa
; */

(defrule ActivarHabitacion
        (declare (salience 909))

        (ultimo_registro movimiento ?h ?i)
        (valor_registrado ?i movimiento ?h on)
        ?n <- (estado_habitacion (habitacion ?h) (estado inactiva | parece_inactiva))
        =>
        (modify ?n (estado activa) (instante ?i))
)

; /*
; * REGLA 5: Si el sensor de movimiento de una habitación está en OFF, habitación parece inactiva
; */

(defrule PareceInactiva
        (declare (salience 909))
        (HoraActualizada ?horaactual)

        (ultimo_registro movimiento ?h ?i)
        (valor_registrado ?i movimiento ?h off)
        ?n <- (estado_habitacion (habitacion ?h) (estado activa) (instante ?ins))
        (test (eq ?i ?horaactual)) ; Para que solo pase a parece_inactiva una vez y evitar bucle infinito
        =>
        (modify ?n (estado parece_inactiva) (instante ?i))
)

; /*
; * REGLA 6: Si una habitación parece inactiva pero es una habitación
; *          que tiene un mínimo de luz muy bajo, pasa a inactiva
; *          (por ejemplo, un dormitorio; es muy probable que si una persona
; *           está ahí y no se mueve, no necesite luz)
; */

(defrule DesactivarHabitacionesPocoProbables
        (declare (salience 899))
        (HoraActualizada ?horaactual)

        ?n <- (estado_habitacion (habitacion ?h) (estado parece_inactiva) (instante ?i))
        (minimo_luz (habitacion ?h) (luz ?l))
        (test (< ?l 200))
        =>
        (modify ?n (estado inactiva) (instante ?horaactual))
)

; /*
; * REGLA 7: Si una habitación parece inactiva y tiene alguna habitación contigua
; *         que esté activa y que tenga un mínimo de luz alto (por ejemplo, cocina-comedor
; *         dormitorio-pasillo),
; *         esa habitación parece activa (es probable que se estén produciendo pasos)
; */

(defrule HabitacionesParecenActivas
        (declare (salience 900))
        (HoraActualizada ?horaactual)

        ?n <- (estado_habitacion (habitacion ?h) (estado parece_inactiva) (instante ?i))

        (posible_pasar ?h ?h2)
        (estado_habitacion (habitacion ?h2) (estado activa) (instante ?i))

        (minimo_luz (habitacion ?h2) (luz ?l))
        (test (> ?l 300))
        =>
        (modify ?n (estado parece_activa) (instante ?horaactual))
)

; /*
; * REGLA 8: Si una habitación parece activa durante más de 30 segundos,
; *         esa habitación pasa a inactiva (es muy poco probable que en una
; *         habitación que parece activa no haya movimiento durante 30 segundos)
; */

(defrule InactivarHabitacionesParecenActivas
        (declare (salience 899))
        (HoraActualizada ?horaactual)

        ?n <- (estado_habitacion (habitacion ?h) (estado parece_activa) (instante ?i))

        (test (> (- ?horaactual ?i) 30))
        =>
        (modify ?n (estado inactiva) (instante ?horaactual))
)

; /*
; * REGLA 9: Si una habitación parece activa y es una habitación
;           con un mínimo de luz bajo, habitación inactiva
; */

(defrule InactivarHabitacionesParecenActivasMinimoLuz
        (declare (salience 900))
        (HoraActualizada ?horaactual)

        ?n <- (estado_habitacion (habitacion ?h) (estado parece_activa) (instante ?i))

        (minimo_luz (habitacion ?h) (luz ?l))
        (test (< ?l 200))
        =>
        (modify ?n (estado inactiva) (instante ?horaactual))
)

; /*
; * REGLA 10: Si una habitación parece inactiva pero las habitaciones contiguas
; *         están inactivas, habitación parece activa
; */

(defrule ActivarParecenInactivas
        (declare (salience 900))
        (HoraActualizada ?horaactual)

        ?n <- (estado_habitacion (habitacion ?h) (estado parece_inactiva) (instante ?i))

        (posible_pasar ?h ?h2)

        (estado_habitacion (habitacion ?h2) (estado inactiva) (instante ?i))

        =>
        (modify ?n (estado parece_activa) (instante ?horaactual))
)
