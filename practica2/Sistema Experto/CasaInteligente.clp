; /******************************************************/
; /                        TEMPLATES                     /
; /******************************************************/
(defglobal ?*hora_inicio_dia* = 43200) ; Desde las 12:00 hasta las
(defglobal ?*hora_fin_dia* = 72000)    ; 20:00

(defglobal ?*hora_inicio_maniana* = 28800) ; Desde las 08:00 hasta las
(defglobal ?*hora_fin_maniana* = 43200)    ; 12:00

(defglobal ?*hora_inicio_tarde* = 43201)   ; Desde las 12:01 hasta las
(defglobal ?*hora_fin_tarde* = 72000)      ; 20:00

(defglobal ?*hora_inicio_noche* = 72001)   ; Desde las 20:01 hasta las
(defglobal ?*hora_fin_noche* = 28799)      ; 07:59

(defglobal ?*hora_inicio_noche_persona_despierta_noche* = 0)  ; Desde las 00:00 hasta las 
(defglobal ?*hora_fin_noche_persona_despierta_noche* = 28800) ; 08:00

(defglobal ?*hora_inicio_llegada_asistenta* = 35100)    ; Desde las 09:45 hasta las 
(defglobal ?*hora_fin_llegada_asistenta* = 36900)       ; 10:15

(defglobal ?*hora_inicio_salida_asistenta* = 49500)  ; Desde las 13:45 hasta las
(defglobal ?*hora_fin_salida_asistenta* = 51300)     ; 14:15

(defglobal ?*hora_inicio_persona_durmiendo_tarde* = 79200) ; Desde las 22:00 hasta las 
(defglobal ?*hora_fin_persona_durmiendo_tarde* = 7200)     ; 02:00

; /*
; * CONOCIMIENTO SOBRE LA CASA:
; *     - Habitacion: nombre de la habitación y número de ventanas
; *     - Puerta: dos habitaciones contiguas con una puerta
; *     - Paso: dos habitaciones contiguas pero sin pared
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

; /*
; * TEMPLATES SOBRE EL ESTADO DE LAS HABITACIONES Y/O PUERTAS
; */

; /* Estado de una habitación (activa, parece_inactiva, inactiva) en un determinado instante
(deftemplate estado_habitacion
        (slot habitacion)
        (slot estado)
        (slot instante)
)

; /* Concepto de posible paso en el conocimiento del profesor
(deftemplate posible_paso
        (slot habitacion1)
        (slot habitacion2)
        (slot instante)
)

; /* Concepto de se ha producido un paso en el conocimiento del profesor
(deftemplate se_ha_producido_paso
        (slot habitacion1)
        (slot habitacion2)
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
                (habitacion dormitorio)
                (ventanas 0)
        )

        (Habitacion
                (habitacion banio)
                (ventanas 2)
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

        (Habitacion
                (habitacion calle)
                (ventanas 0)
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
                (puerta_habitacion2 dormitorio)
        )

        (Puerta
                (puerta_habitacion1 pasillo)
                (puerta_habitacion2 banio)
        )

        (Puerta
                (puerta_habitacion1 pasillo)
                (puerta_habitacion2 despensa)
        )

        (Puerta
                (puerta_habitacion1 despensa)
                (puerta_habitacion2 garaje)
        )

        (Puerta
                (puerta_habitacion1 dormitorio)
                (puerta_habitacion2 salon)
        )

        (Puerta
                (puerta_habitacion1 entrada)
                (puerta_habitacion2 calle)
        )
)

(deffacts Pasos
        (Paso
                (paso_habitacion1 salon)
                (paso_habitacion2 comedor)
        )
)

; /* - Inicialmente está la persona mayor en casa (1 persona en casa)
; /* - El día de la semana es lunes
(deffacts Otros
        (numero_de_personas 1)
        (dia_de_la_semana lunes)
        (asistenta_en_casa false)
)

; El número de pasos aproximado que una persona ha realizado desde las 00:00 hasta la hora indicada
; (NumeroPasosNormales ?numero_de_pasos_aproximado ?hora)
(deffacts ActividadPersonaNormal
        (NumeroPasosNormales 5 36000)   ; Quiere decir que desde las 00:00 hasta las 10:00 (36000 segundos) la persona realiza alrededor de 5 pasos
        (NumeroPasosNormales 15 37800)
        (NumeroPasosNormales 25 39600)
        (NumeroPasosNormales 35 41400)
        (NumeroPasosNormales 45 43200)
        (NumeroPasosNormales 55 45000)
        (NumeroPasosNormales 65 46800)
        (NumeroPasosNormales 75 48600)
        (NumeroPasosNormales 85 50400)
        (NumeroPasosNormales 95 52200)
        (NumeroPasosNormales 105 54000)
)

; El número de pasos aproximado que una persona ha realizado en una franja horaria
; (NumeroPasosNormalesFranja ?numero_de_pasos_aproximado franja_maniana/tarde/noche)
(deffacts ActividadPorFranjas
        (NumeroPasosNormalesFranja 120 franja_maniana)   
        (NumeroPasosNormalesFranja 200 franja_tarde)
        (NumeroPasosNormalesFranja 50 franja_noche)
)

; /* Inicialmente todas las habitaciones tienen la ultima_desactivacion en el instante 0

(defrule IniciarUltimaDesactivacion
        (declare (salience 10000))
        (Habitacion (habitacion ?h))
        (not (ultima_desactivacion movimiento ?h ?instante))

        =>
        (assert (ultima_desactivacion movimiento ?h 0))
)

; /* Inicialmente todas las habitaciones tengan la ultima_activacion en el instante 0

(defrule IniciarUltimaActivacion
        (declare (salience 10000))
        (Habitacion (habitacion ?h))
        (not (ultima_activacion movimiento ?h ?instante))

        =>
        (assert (ultima_activacion movimiento ?h 0))
)

; /* Inicialmente todas las habitaciones están inactivas

(defrule EstadoHabitacionesInicial
        (declare (salience 10000))
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

; /*
; *  Si se puede pasar directamente (por una puerta o paso)
; *  de una habitación a otra, añadir a la base de hechos
; *  (posible pasar habitacion1 habitacion2)
; */

; /* Si h1 y h2 tienen una puerta, es posible pasar desde h1 a h2 y viceversa
(defrule PosiblePasarPuertas
        (declare (salience 9999))
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
        (declare (salience 9999))
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
; * Si para acceder a una habitación solo se puede pasar desde otra,
; * añadir a la base de hechos
; * (necesario_pasar habitacion1 habitacion2)
; */

; /* PASO 1
; * Encontrar las habitaciones que tienen más de una puerta (conectan con más
; * de una habitación)
; * Si una habitación h1 conecta con h2 y con otra h3,
; * h1 tiene más de una puerta
; */
(defrule HabitacionesConMasDeUnaPuerta
        (declare (salience 9998))
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
        (declare (salience 9997))
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
        (declare (salience 9997))
        (habitacion_con_una_sola_puerta ?H1)

        (posible_pasar ?H1 ?H2)
        =>
        (assert (necesario_pasar ?H1 ?H2))
)

; /* Registro del sensor de movimiento cuando está en ON
; /* 1) Añado el valor valor_registrado
; /* 2) Añado último registro
; /* 3) Añado última activación
; /* 4) Si la persona está sola y la hora está entre 12:00 y 20:00, añadir ultimo_movimiento
;       FUNCIONALIDAD 2: Siendo de día, la persona no se ha movido por la casa en las últimas 3 horas
; /* 5) Si la persona está sola y la hora está entre 00:00 y 08:00, añadir primer_movimiento_noche y
;       activar módulo persona_despierta_noche
;       FUNCIONALIDAD 3: La persona se ha levantado durante más de 15 minutos por la noche
; /* 6) Si la persona está sola y entra al baño, añadir instante_entra_banio y activar módulo
;       persona_mucho_tiempo_banio
;       FUNCIONALIDAD 4: Estando sola, la persona lleva más 20 minutos en el baño

(defrule SensorMovimientoOn
        (declare (salience 9999))
        ?n <- (valor movimiento ?habitacion on)
        (numero_de_personas ?numero)
        =>
        (bind ?instante (totalsegundos ?*hora* ?*minutos* ?*segundos*))

        (assert (valor_registrado ?instante movimiento ?habitacion on))

        (assert (ultimo_registro movimiento ?habitacion ?instante))

        (assert (ultima_activacion movimiento ?habitacion ?instante))

        ; Hora entre las 12:00 y 20:00 (DE DÍA)
        (if (and (> ?instante ?*hora_inicio_dia*) (< ?instante ?*hora_fin_dia*) (eq ?numero 1))
            then
                (assert (ultimo_movimiento ?instante))
        )

        ; Hora entre las 00:00 y 08:00
        (if (and (> ?instante ?*hora_inicio_noche_persona_despierta_noche*) (< ?instante ?*hora_fin_noche_persona_despierta_noche*) (eq ?numero 1))
            then
                (assert (modulo persona_despierta_noche))
                (assert (primer_movimiento_noche ?instante))
        )

        ; Si la persona está sola y entra al baño
        (if (and (eq ?habitacion banio) (eq ?numero 1))
            then
                (assert (modulo persona_mucho_tiempo_banio))
                (assert (instante_entra_banio ?instante))
        )

        (retract ?n)
)

; /* Registro del sensor de movimiento cuando está en OFF
; /* 1) Añado el valor valor_registrado
; /* 2) Añado último registro
; /* 3) Añado última desactivación
; /* 6) Si la persona entra al baño, añadir persona_entra_banio
;       FUNCIONALIDAD 6: La persona ha ido al baño varias veces en las últimas 3 horas
(defrule SensorMovimientoOff
        (declare (salience 9999))
        ?n <- (valor movimiento ?habitacion off)

        (numero_de_personas ?numero)
        =>
        (bind ?instante (totalsegundos ?*hora* ?*minutos* ?*segundos*))

        (assert (valor_registrado ?instante movimiento ?habitacion off))

        (assert (ultimo_registro movimiento ?habitacion ?instante))

        (assert (ultima_desactivacion movimiento ?habitacion ?instante))

        ; Contar número de veces que se va al baño sólo si la persona está sola en casa
        (if (and (eq ?habitacion banio) (eq ?numero 1)) then
                (assert (persona_entra_banio ?instante))
        )

        (retract ?n)
)

; /* Registro del sensor de luminosidad
; /* 1) Añado el valor valor_registrado
; /* 2) Añado último registro
(defrule SensorLuminosidad
        (declare (salience 9999))
        ?n <- (valor luminosidad ?habitacion ?l)
        =>
        (bind ?instante (totalsegundos ?*hora* ?*minutos* ?*segundos*))

        (assert (valor_registrado ?instante luminosidad ?habitacion ?l))

        (assert (ultimo_registro luminosidad ?habitacion ?instante))

        (retract ?n)
)

; /* Registro del sensor de la puerta
; /* 1) Añado el valor valor_registrado
; /* 2) Añado último registro
; /* 3) Añado última activación

(defrule SensorPuerta
        (declare (salience 9999))
        ?n <- (valor puerta ?h1 ?h2 ?val)
        =>
        (bind ?instante (totalsegundos ?*hora* ?*minutos* ?*segundos*))

        (assert (valor_registrado ?instante puerta ?h1 ?h2 ?val))

        (assert (ultimo_registro puerta ?h1 ?h2 ?instante))

        (assert (modulo persona_ha_salido_de_casa))

        (retract ?n)
)

; /* 1) De entre todos los hechos (ultimo_movimiento ?instante)
; /*    se coge el último movimiento con el tiempo máximo
; /* 2) Se borran los hechos (ultimo_movimiento ?i) que sean menores
; /*    que el tiempo máximo del paso 1)

(defrule UltimoMovimiento
        (declare (salience 9998))
        (ultimo_movimiento ?instante)
        (not (valor_registrado ?value2&:(maximo ?value2 ?instante)))
        ?n <- (ultimo_movimiento ?i)
        (test (< ?i ?instante))
        =>
        (retract ?n)
)

; /* 1) De entre todos los hechos (valor_registrado ?instante ?m ?hab ?res)
; /*    se coge el valor registrado con el tiempo máximo
; /* 2) Se borran los hechos (ultimo_registro ?m ?hab ?i) que sean menores
; /*    que el tiempo máximo del paso 1)

(defrule UltimoRegistro
        (declare (salience 9998))
        (valor_registrado ?instante ?m ?hab ?res)
        (not (valor_registrado ?value2&:(maximo ?value2 ?instante) ?m ?hab ?res))
        ?n <- (ultimo_registro ?m ?hab ?i)
        (test (< ?i ?instante))
        =>
        (retract ?n)
)

; /* 1) De entre todos los hechos (valor_registrado ?instante ?m ?hab ?res)
; /*    se coge el valor registrado con el tiempo máximo
; /* 2) Se borran los hechos (ultimo_registro ?m ?hab ?i) que sean menores
; /*    que el tiempo máximo del paso 1)

(defrule UltimoRegistroSensorPuerta
        (declare (salience 9998))
        (valor_registrado ?instante ?m ?h1 ?h2 ?res)
        (not (valor_registrado ?value2&:(maximo ?value2 ?instante) ?m ?h1 ?h2 ?res))
        ?n <- (ultimo_registro ?m ?h1 ?h2 ?i)
        (test (< ?i ?instante))
        =>
        (retract ?n)
)

; /*
; * Registrar última activación, última vez que el sensor de movimiento pasó de OFF a ON
; */
; /* PASO 1
; * 1) Se coge el momento en el que se produjo la última desactivación
; * 2) Se cogen los hechos (ultima_activacion) cuyo instante sea mayor que
; *     el momento del paso 1)
; * 3) De entre todos los hechos del paso 2), se coge el que tenga el mínimo tiempo
; * 4) Se borran los hechos del paso 2) que no sean mínimos
; */

(defrule UltimaActivacion1
        (declare (salience 9997))
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
        (declare (salience 9996))
        (ultima_activacion movimiento ?hab ?instante)
        (not (ultima_activacion movimiento ?hab ?value2&:(maximo ?value2 ?instante)))
        ?borrar1 <- (ultima_activacion movimiento ?hab ?i)
        (test (< ?i ?instante))

        =>

        (retract ?borrar1)
)

; /*
; * Registrar última desactivación, última vez que el sensor demovimiento pasó de ON a OFF
; */
; /*
; * 1) Se coge el máximo instante de los hechos (valor_registrado movimiento off)
; * 2) Se borran los hechos (ultima_desactivacion) cuyo instante sea menor
; *     que el máximo del paso 1)
; */

(defrule UltimaDesactivacion
        (declare (salience 9997))
        (valor_registrado ?instante movimiento ?hab off)
        (not (valor_registrado ?value2&:(maximo ?value2 ?instante) movimiento ?hab off))
        ?n <- (ultima_desactivacion movimiento ?hab ?i)
        (test (< ?i ?instante))
        =>
        (retract ?n)
)

; /*
; * Si el sensor de movimiento de una habitación está en ON, habitación activa
; */

(defrule ActivarHabitacion
        (declare (salience 9000))

        (ultimo_registro movimiento ?h ?i)
        (valor_registrado ?i movimiento ?h on)
        ?n <- (estado_habitacion (habitacion ?h) (estado inactiva | parece_inactiva))
        =>
        (modify ?n (estado activa) (instante ?i))
)

; /*
; * Si el sensor de movimiento de una habitación está en OFF, habitación parece inactiva
; */

(defrule PareceInactiva
        (declare (salience 9000))
        (HoraActualizada ?horaactual)

        (ultimo_registro movimiento ?h ?i)
        (valor_registrado ?i movimiento ?h off)
        ?n <- (estado_habitacion (habitacion ?h) (estado activa) (instante ?ins))
        (test (eq ?i ?horaactual)) ; Para que solo pase a parece_inactiva una vez y evitar bucle infinito
        =>
        (modify ?n (estado parece_inactiva) (instante ?i))
)

; /*
; * Si una habitación parece inactiva durante más de 10 segundos, habitación inactiva
; */

(defrule DesactivarHabitacion
        (declare (salience 8900))
        (HoraActualizada ?horaactual)

        ?n <- (estado_habitacion (habitacion ?h) (estado parece_inactiva) (instante ?i))
        (test (> (- ?horaactual ?i) 10))
        =>
        (modify ?n (estado inactiva) (instante ?horaactual))
)

; /*
; * Al disparar el sensor de movimiento de una habitación, registro
; * que se ha podido producir un paso desde las habitaciones que sean
; * accesibles y que estuvieran activas o recientemente activas
; */

(defrule PosiblePaso
        (declare (salience 907))
        (HoraActualizada ?horaactual)
        (ultimo_registro movimiento ?h1 ?i)
        (valor_registrado ?i movimiento ?h1 on)

        (posible_pasar ?h2 ?h1)

        (estado_habitacion (habitacion ?h2) (estado activa | parece_inactiva) (instante ?ins))
        (test (> ?i ?ins))
        (test (eq ?i ?horaactual))
        =>
        (assert (posible_paso (habitacion1 ?h2) (habitacion2 ?h1) (instante ?i)))
)

; /*
; * En relación a la regla anterior, si sólo hay una posible habitación,
; * se deduce que se ha producido el paso
; */

(defrule SeHaProducidoPaso
        (declare (salience 906))
        (numero_de_personas ?numero)
        (posible_paso (habitacion1 ?h1) (habitacion2 ?h2) (instante ?i))
        (not (posible_paso (habitacion1 ?h3 & ~?h1) (habitacion2 ?h2) (instante ?i)))
        ?n <- (posible_paso (habitacion1 ?h1) (habitacion2 ?h2) (instante ?i))
        =>
        (retract ?n)
        (if (eq ?numero 1) then
                (assert (se_ha_producido_paso (habitacion1 ?h1) (habitacion2 ?h2) (instante ?i)))
        )
)

; /*
; * MÓDULO PARA SABER SI LA PERSONA HA SALIDO DE CASA
; */

; /*
; *  Regla para saber si se ha abierto la puerta que da a la calle
; *  1) Ver el último registro del sensor magnético de la puerta
; *  2) Comprobar que el sensor magnético está en OFF (puerta abierta)
; *  3) Comprobar si la puerta que se ha abierto es de la calle
; */
(defrule Funcionalidad1
        ?f <- (modulo persona_ha_salido_de_casa)
        (HoraActualizada ?i)
        (ultimo_registro puerta ?h1 ?h2 ?i)
        (valor_registrado ?i puerta ?h1 ?h2 off)
        (test (or (eq ?h1 calle) (eq ?h2 calle)))
        =>
        (assert (PuertaCalleAbierta ?i))
        (retract ?f)     
)

; /*
; *  Regla para deducir si alguien entra a casa
; *  1) Para entrar en la regla debe existir el hecho PuertaCalleAbierta
; *  2) Ver las habitaciones que están conectadas con la calle
; *  3) Si la habitación conectada a la calle está inactiva, significa que parece
;       que alguien está entrando desde la calle
; */
(defrule DeducirEntradaACasa
        ?f <- (PuertaCalleAbierta ?i)
        (posible_pasar ?h calle)
        (estado_habitacion (habitacion ?h) (estado inactiva))
        =>
        (bind ?instante (totalsegundos ?*hora* ?*minutos* ?*segundos*))

        (assert (parece_que_alguien_entra ?h ?instante))
        (retract ?f)
)

; /*
; *  Regla para deducir si alguien entra a casa
; *  1) Para entrar en la regla debe existir el hecho (parece_que_alguien_entra)
; *  2) Si la habitación que conecta con la calle se activa durante los próximos 3
;       segundos, se deduce que alguien ha entrado desde la calle a esa habitación
; */

(defrule DeducirEntradaACasa2
        (HoraActualizada ?i)
        ?f <- (parece_que_alguien_entra ?h ?instante)
        ?n <- (numero_de_personas ?numero)
        (estado_habitacion (habitacion ?h) (estado activa) (instante ?i))
        (test (< (- ?i ?instante) 3))
        (test (> (- ?i ?instante) 0))
        (dia_de_la_semana ?dia)
        ?f2 <- (asistenta_en_casa ?v)

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        =>

        (if (and (eq ?v false) (eq ?numero 1) (> ?i ?*hora_inicio_llegada_asistenta*) (< ?i ?*hora_fin_llegada_asistenta*) (not (eq ?dia domingo))) then
                (printout t ?h1 ":" ?m1 ":" ?s1 ": Ha entrado la asistenta" crlf)
                (retract ?f2)
                (assert (asistenta_en_casa true))

        else
                (if (eq ?numero 0) then
                        (printout t ?h1 ":" ?m1 ":" ?s1 ": Ha entrado la persona mayor" crlf)
                
                else

                        (printout t ?h1 ":" ?m1 ":" ?s1 ": Ha entrado otra persona" crlf)
                )
        )

        (retract ?f)
        (retract ?n)
        (assert (numero_de_personas (+ ?numero 1)))
)

; /*
; *  Regla para eliminar los hechos (parece_que_alguien_entra) que lleven más de 3 segundos
; *  1) Para entrar en la regla debe existir el hecho (parece_que_alguien_entra)
; *  2) Si el hecho (parece_que_alguien_entra) lleva más de 3 segundos en la base de hechos, se elimina
; */

(defrule DeducirEntradaACasa3
        ?f <- (parece_que_alguien_entra ?h ?instante)
        (HoraActualizada ?i)
        (test (> (- ?i ?instante) 3))
        =>
        (retract ?f)
)

; /*
; *  Regla para para deducir si alguien sale a la calle
; *  1) Para entrar en la regla debe existir el hecho (PuertaCalleAbierta)
; *  2) Si la habitación que conecta con la calle está activa, se deduce que alguien va a salir a la calle
; */
(defrule DeducirSalidaACalle
        ?f <- (PuertaCalleAbierta ?instante)
        (posible_pasar ?h calle)
        (estado_habitacion (habitacion ?h) (estado activa) (instante ?i))
        (test (< ?i ?instante))
        ?n <- (numero_de_personas ?numero)
        (dia_de_la_semana ?dia)
        ?f2 <- (asistenta_en_casa ?v)

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        =>
        ; Si el número de personas es 2 y la hora está entre 13:45 y 14:15
        (if (and (eq ?v true) (eq ?numero 2) (> ?i ?*hora_inicio_salida_asistenta*) (< ?i ?*hora_fin_salida_asistenta*) (not (eq ?dia domingo))) then
                (printout t ?h1 ":" ?m1 ":" ?s1 ": Ha salido la asistenta" crlf)
                (retract ?f2)
                (assert (asistenta_en_casa false))
        
        else
                (if (eq ?numero 1) then
                        (printout t ?h1 ":" ?m1 ":" ?s1 ": La persona mayor ha salido a la calle" crlf)

                else
                        (printout t ?h1 ":" ?m1 ":" ?s1 ": Ha salido otra persona" crlf)
                )
        )
        (retract ?f)
        (retract ?n)
        (assert (numero_de_personas (- ?numero 1)))
)

; /*
; * MÓDULO PARA COMPROBRAR SI LA PERSONA NO SE HA MOVIDO POR LA CASA EN
; * LAS ÚLTIMAS 3 HORAS SIENDO DE DÍA
; */

; /*
; *  Regla para comprobar si el último movimiento de la persona se hizo
; *  hace más de 3 horas
; *  1) Comprobar si la diferencia entre el último movimiento y la hora actual
; *     es de hace más de 3 horas (10800 segundos)
; */
(defrule Funcionalidad2
        ?f <- (modulo persona_no_se_ha_movido)
        (HoraActualizada ?horaactual)
        ?f2 <- (ultimo_movimiento ?i)
        (test (> (- ?horaactual ?i) 10800))
        (numero_de_personas ?numero)

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        =>

        (if (and (> ?horaactual ?*hora_inicio_dia*) (< ?horaactual ?*hora_fin_dia*) (eq ?numero 1))
            then
                (printout t ?h1 ":" ?m1 ":" ?s1 ": La persona lleva sin moverse más de 3 horas" crlf)
        )
        
        (retract ?f)  
        (retract ?f2)   
)

; /*
; *     Regla para borrar los últimos movimientos cuando NO es de día (porque ya no sirven)
; */
(defrule BorrarFuncionalidad2
        ?f <- (ultimo_movimiento ?i)
        (HoraActualizada ?horaactual)

        (test (and (> ?horaactual ?*hora_inicio_dia*) (< ?horaactual ?*hora_fin_dia*)))

        =>

        (retract ?f)  
)

; /*
; * MÓDULO PARA COMPROBRAR SI LA PERSONA SE HA DESPERTADO DURANTE
; * MÁS DE 15 MINUTOS POR LA NOCHE
; */

; /*
; *  Regla para comprobar si el último registro de movimiento de la persona se hizo
; *  hace más de 15 minutos con respecto al primer movimiento que hizo por la noche
; */
(defrule Funcionalidad3
        ?f <- (modulo persona_despierta_noche)
        (ultimo_registro movimiento ?h ?instante)
        ?f2 <- (primer_movimiento_noche ?i)
        (test (> (- ?instante ?i) 900))
        (numero_de_personas ?numero)

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        =>
        (if (and (< ?instante ?*hora_inicio_noche_persona_despierta_noche*) (> ?instante ?*hora_fin_noche_persona_despierta_noche*) (eq ?numero 1))
            then
                (printout t ?h1 ":" ?m1 ":" ?s1 ": La persona lleva más de 15 minutos despierta" crlf)
        )
        
        (retract ?f)
        (retract ?f2)
)

; /*
; *  Si el módulo está activo y todas las habitaciones inactivas, deduzco que 
; *  la persona se ha ido a dormir
; */
(defrule BorrarFuncionalidad3
        ?f <- (modulo persona_despierta_noche)
        ?f2 <- (primer_movimiento_noche ?i)
        (not (estado_habitacion (estado activa)))

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        =>
        (printout t ?h1 ":" ?m1 ":" ?s1 ": La persona está dormida" crlf)
        (retract ?f)
        (retract ?f2)
)

; /*
; *  Borrar los assert (primer_movimiento_noche) que no sean el primer
; * movimiento de la noche
; */
(defrule BorrarPrimerMovimientoNoche
        (primer_movimiento_noche ?i)
        (not (primer_movimiento_noche ?value2&:(minimo ?value2 ?i)))
        ?borrar1 <- (primer_movimiento_noche ?instante)
        (test (< ?i ?instante))
        =>
        (retract ?borrar1)
)

; /*
; *  Borrar los assert (primer_movimiento_noche) si la hora del sistema
; *  no está en el intervalo de noche
; */
(defrule BorrarPrimerMovimientoNoche2
        ?f <- (primer_movimiento_noche ?i)
        (HoraActualizada ?horaactual)

        (test (and (< ?horaactual ?*hora_inicio_noche_persona_despierta_noche*) (> ?horaactual ?*hora_fin_noche_persona_despierta_noche*)))
        =>
        (retract ?f)
)

; /*
; * MÓDULO PARA COMPROBRAR SI LA PERSONA ESTANDO SOLA LLEVA MÁS
; * DE 20 MINUTOS EN EL BAÑO
; */

; /*
; *  Regla para comprobar si el instante en el que entró al baño fue hace más de
; *  20 minutos
; */
(defrule Funcionalidad4
        ?f <- (modulo persona_mucho_tiempo_banio)
        (HoraActualizada ?hora)
        ?f2 <- (instante_entra_banio ?instante)
        (test (> (- ?hora ?instante) 1200))

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        =>
        (printout t ?h1 ":" ?m1 ":" ?s1 ": La persona lleva más de 20 minutos en el banio" crlf)
        (retract ?f)
        (retract ?f2)
)

; /*
; *  Si el módulo está activo y se enciende otra habitación que no sea el baño,
; *  deduzco que la persona ha salido del baño
; */
(defrule BorrarFuncionalidad4
        (declare (salience 1000))
        ?f <- (modulo persona_mucho_tiempo_banio)
        ?f2 <- (instante_entra_banio ?i)
        (ultimo_registro movimiento ?h ?instante)
        (posible_pasar ?h banio)
        (test (neq ?h banio))
        (test (> ?instante ?i))

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        =>
        (printout t ?h1 ":" ?m1 ":" ?s1 ": La persona ha salido del banio" crlf)
        (retract ?f)
        (retract ?f2)
)

; /*
; *  Borrar los assert (instante_entra_banio) que no sean el primer
; *  momento que entró al baño
; */
(defrule BorrarInstanteEntradaBanio
        (instante_entra_banio ?i)
        (not (instante_entra_banio ?value2&:(minimo ?value2 ?i)))
        ?borrar1 <- (instante_entra_banio ?instante)
        (test (< ?i ?instante))
        =>
        (retract ?borrar1)
)

; /*
; * MÓDULO PARA COMPROBRAR SI LA PERSONA NO SE HA IDO AL BANIO
; * EN LAS ÚLTIMAS 12 HORAS
; */

; /*
; *  Regla para comprobar si la última vez que el baño tuvo movimiento
; *  fue hace más de 12 horas
; *  1) Comprobar si la diferencia entre el último registro de movimiento
; *  del baño y la hora actua es más de 12 horas (43200 segundos)
; */
(defrule Funcionalidad5
        ?f <- (modulo persona_no_ha_ido_banio)
        (HoraActualizada ?hora)
        (ultimo_registro movimiento banio ?i)
        (test (> (- ?hora ?i) 43200))

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        
        =>
        (printout t ?h1 ":" ?m1 ":" ?s1 ": La persona lleva sin ir al banio más de 12 horas" crlf)
        (retract ?f)  
)

; /*
; * MÓDULO PARA COMPROBRAR SI LA PERSONA HA IDO VARIAS VECES
; * AL BAÑO EN LAS ÚLTIMAS 3 HORAS
; */

; Variable global para contar las veces que ha ido al baño
(defglobal ?*n_veces_banio* = 0) 

; /*
; *  Regla para activar contador de las veces que ha ido al baño
; */
(defrule Funcionalidad6
        (declare (salience 1000))
        ?f <- (modulo persona_varias_veces_banio)

        =>

        (assert (ContarHechos))
        (retract ?f)
)

; Cuando en la base de hechos hay (ContarHechos),
; se cuentan los hechos del tipo (persona_entra_banio)
(defrule Contador6
        (declare (salience 999))
        (ContarHechos)

        (HoraActualizada ?i)
        (persona_entra_banio ?hora)
        (test (< (- ?i ?hora) 10800))

        =>
        
        (bind ?*n_veces_banio* (+ ?*n_veces_banio* 1))
)

; Una vez se han contado todos los hechos, si todavía no hay ningún hecho
; (NumeroVecesBanio), significa que es la primera vez que se va a insertar
; un hecho de este tipo, con lo cual no hay que eliminar ningún hecho anterior.
; El contador global se resetea a 0
(defrule ResultadoPrimero
        (declare (salience 10))
        
        (not (NumeroVecesBanio ?))

        ?eliminar1 <- (ContarHechos)        

        =>
        
        (assert (NumeroVecesBanio ?*n_veces_banio*))
        (retract ?eliminar1)
        (bind ?*n_veces_banio* 0)
)

; Si hay algún hecho del tipo (persona_entra_banio), se elimina
; y se añade un nuevo hecho con el contador correcto
; El contador global se resetea a 0
(defrule Resultado6
        (declare (salience 10))

        ?eliminar1 <- (NumeroVecesBanio ?)
        
        ?eliminar2 <- (ContarHechos)        

        =>
        
        (retract ?eliminar1)
        (retract ?eliminar2)

        (assert (NumeroVecesBanio ?*n_veces_banio*))

        (bind ?*n_veces_banio* 0)
)

; /*
; *  Borrar los assert (persona_entra_banio) que hayan pasado hace más
; *  de 3 horas
; */
(defrule BorrarPrimeraEntradaBanio

        (HoraActualizada ?i)
        ?f <- (persona_entra_banio ?hora)
        (test (> (- ?i ?hora) 10800))
        =>
        (retract ?f)
)

; /*
; *  Regla para deducir si la persona ha ido varias veces al baño
; *  en las últimas 3 horas
; */
(defrule DeducirPersonaVariasVecesBanio
        (declare (salience -10))

        (NumeroVecesBanio ?n)
        (test (> ?n 5))
        =>
        (printout t "La persona ha ido varias veces al banio en las ultimas 3 horas" crlf)
)

; /*
; * MÓDULO PARA SABER SI LA ASISTENTA LLEGA TARDE
; */

(defrule Funcionalidad7
        (declare (salience -1000))
        ?f <- (modulo asistenta_llega_tarde)
        (HoraActualizada ?h)
        (test (eq ?h (+ ?*hora_fin_llegada_asistenta* 1)) )
        (asistenta_en_casa false)
        (dia_de_la_semana ?d)

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        =>

        (if (neq ?d domingo) then
                (printout t ?h1 ":" ?m1 ":" ?s1 ": La asistenta llega tarde" crlf)
        )

        (retract ?f)
)

; /*
; * MÓDULO PARA SABER SI LA PERSONA SE HA IDO A DORMIR TARDE
; */

(defrule Funcionalidad8
        (declare (salience -1000))
        ?f <- (modulo persona_no_se_ha_dormido)
        (HoraActualizada ?hora)
        (not (estado_habitacion (estado activa)))

        (ultima_activacion movimiento ?hab ?instante)
        (not (ultima_activacion movimiento ?hab ?value2&:(maximo ?value2 ?instante)))

        (test (neq ?hab dormitorio))

        (numero_de_personas ?n)

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        =>
        (if (and (> ?hora ?*hora_inicio_persona_durmiendo_tarde*) (< ?instante ?*hora_fin_persona_durmiendo_tarde*) (eq ?n 1))
            then
                (printout t ?h1 ":" ?m1 ":" ?s1 ": Es tarde y la persona no se ha ido a dormir" crlf)
        )
        (retract ?f)
)

; /*
; * MÓDULO PARA COMPROBAR SI LA PERSONA TIENE MENOS ACTIVIDAD
; * DE LO NORMAL
; */

; Variable global para contar las veces que la persona ha pasado de habitación
(defglobal ?*n_pasos* = 0) 

; /*
; *  Regla para activar contador
; */
(defrule Funcionalidad9
        (declare (salience 1000))
        ?f <- (modulo persona_inactiva)

        =>

        (assert (ContarHechosPersonaInactiva))
        (retract ?f)
)

; Cuando en la base de hechos hay (ContarHechosPersonaInactiva),
; se cuentan los hechos del tipo (se_ha_producido_paso) de las últimas 12 horas (43200 segundos)
(defrule Contador9
        (declare (salience 999))
        (ContarHechosPersonaInactiva)

        (HoraActualizada ?i)
        (se_ha_producido_paso (instante ?hora))
        (test (< (- ?i ?hora) 43200))

        =>
        
        (bind ?*n_pasos* (+ ?*n_pasos* 1))
)

; Una vez se han contado todos los hechos, si todavía no hay ningún hecho
; (NumeroVecesBanio), significa que es la primera vez que se va a insertar
; un hecho de este tipo, con lo cual no hay que eliminar ningún hecho anterior.
; El contador global se resetea a 0
(defrule ResultadoPrimero9
        (declare (salience 10))
        
        (not (NumeroPasos ?))

        ?eliminar1 <- (ContarHechosPersonaInactiva)        

        =>
        
        (assert (NumeroPasos ?*n_pasos*))
        (retract ?eliminar1)
        (bind ?*n_pasos* 0)
)

; Si hay algún hecho del tipo (persona_entra_banio), se elimina
; y se añade un nuevo hecho con el contador correcto
; El contador global se resetea a 0
(defrule Resultado9
        (declare (salience 10))

        ?eliminar1 <- (NumeroPasos ?)
        
        ?eliminar2 <- (ContarHechosPersonaInactiva)        

        =>
        
        (retract ?eliminar1)
        (retract ?eliminar2)

        (assert (NumeroPasos ?*n_pasos*))

        (bind ?*n_pasos* 0)
)

; /*
; *  Borrar los assert (se_ha_producido_paso) que hayan pasado hace más
; *  de 12 horas
; */
(defrule BorrarPasos

        (HoraActualizada ?i)
        ?f <- (se_ha_producido_paso (instante ?hora))
        (test (> (- ?i ?hora) 43200))
        =>
        (retract ?f)
)

; /*
; *  Regla para deducir si la persona tiene menos actividad de lo normal
; */
(defrule DeducirPersonaInactiva
        (declare (salience -10))
        (HoraActualizada ?i)
        (NumeroPasosNormales ?n_pasos ?hora)
        (test (eq ?i ?hora))

        (NumeroPasos ?n)
        (test (< ?n ?n_pasos))

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        =>
        (printout t ?h1 ":" ?m1 ":" ?s1 ": La persona tiene menos actividad de lo normal" crlf)
)

; /*
; * MÓDULO PARA COMPROBAR SI LA PERSONA HACE MÁS DESPLAZAMIENTOS
; * DE LO NORMAL EN UNA FRANJA HORARIA
; */

; Variable global para contar las veces que la persona ha pasado de habitación
(defglobal ?*n_pasos_franja_maniana* = 0) 
(defglobal ?*n_pasos_franja_tarde* = 0) 
(defglobal ?*n_pasos_franja_noche* = 0) 

; /*
; *  Regla para activar contador
; */
(defrule Funcionalidad10
        (declare (salience 1000))
        ?f <- (modulo persona_realiza_mas_desplazamientos)
        (HoraActualizada ?hora)
        =>
        (if (and (> ?hora ?*hora_inicio_maniana*) (< ?hora ?*hora_fin_maniana*)) then
                (assert (ContarHechosMasDesplazamientosFranjaManiana)) 

        else
                (if (and (> ?hora ?*hora_inicio_tarde*) (< ?hora ?*hora_fin_tarde*)) then
                        (assert (ContarHechosMasDesplazamientosFranjaTarde)) 

                else
                        (if (and (> ?hora ?*hora_inicio_noche*) (< ?hora ?*hora_fin_noche*)) then
                                (assert (ContarHechosMasDesplazamientosFranjaNoche)) 
                        
                        )
                ) 
        )
        
        (retract ?f)
)

; Cuando en la base de hechos hay (ContarHechosMasDesplazamientos),
; se cuentan los hechos del tipo (se_ha_producido_paso) de las últimas 12 horas (43200 segundos)
(defrule ContadorFranjaManiana10
        (declare (salience 999))
        (ContarHechosMasDesplazamientosFranjaManiana)
        
        (se_ha_producido_paso (instante ?hora))

        =>
        
        (bind ?*n_pasos_franja_maniana* (+ ?*n_pasos_franja_maniana* 1))
)

; Cuando en la base de hechos hay (ContarHechosPersonaInactiva),
; se cuentan los hechos del tipo (se_ha_producido_paso) de las últimas 12 horas (43200 segundos)
(defrule ContadorFranjaTarde10
        (declare (salience 999))
        (ContarHechosMasDesplazamientosFranjaTarde)

        (se_ha_producido_paso (instante ?hora))

        =>
        
        (bind ?*n_pasos_franja_tarde* (+ ?*n_pasos_franja_tarde* 1))
)

; Cuando en la base de hechos hay (ContarHechosPersonaInactiva),
; se cuentan los hechos del tipo (se_ha_producido_paso) de las últimas 12 horas (43200 segundos)
(defrule ContadorFranjaNoche10
        (declare (salience 999))
        (ContarHechosMasDesplazamientosFranjaNoche)

        (se_ha_producido_paso (instante ?hora))

        =>
        
        (bind ?*n_pasos_franja_noche* (+ ?*n_pasos_franja_noche* 1))
)

; Una vez se han contado todos los hechos, si todavía no hay ningún hecho
; (NumeroPasosManiana), significa que es la primera vez que se va a insertar
; un hecho de este tipo, con lo cual no hay que eliminar ningún hecho anterior.
; El contador global se resetea a 0
(defrule ResultadoPrimeroFranjaManiana
        (declare (salience 10))
        
        (not (NumeroPasosManiana ?))

        ?eliminar1 <- (ContarHechosMasDesplazamientosFranjaManiana)        

        =>
        
        (assert (NumeroPasosManiana ?*n_pasos_franja_maniana*))
        (retract ?eliminar1)
        (bind ?*n_pasos_franja_maniana* 0)
)

; Si hay algún hecho del tipo (NumeroPasosManiana), se elimina
; y se añade un nuevo hecho con el contador correcto
; El contador global se resetea a 0
(defrule ResultadoFranjaManiana
        (declare (salience 10))

        ?eliminar1 <- (NumeroPasosManiana ?)
        
        ?eliminar2 <- (ContarHechosMasDesplazamientosFranjaManiana)        

        =>
        
        (retract ?eliminar1)
        (retract ?eliminar2)

        (assert (NumeroPasosManiana ?*n_pasos_franja_maniana*))

        (bind ?*n_pasos_franja_maniana* 0)
)

; Una vez se han contado todos los hechos, si todavía no hay ningún hecho
; (NumeroPasosTarde), significa que es la primera vez que se va a insertar
; un hecho de este tipo, con lo cual no hay que eliminar ningún hecho anterior.
; El contador global se resetea a 0
(defrule ResultadoPrimeroFranjaTarde
        (declare (salience 10))
        
        (not (NumeroPasosTarde ?))

        ?eliminar1 <- (ContarHechosMasDesplazamientosFranjaTarde)        

        =>
        
        (assert (NumeroPasosTarde ?*n_pasos_franja_tarde*))
        (retract ?eliminar1)
        (bind ?*n_pasos_franja_tarde* 0)
)

; Si hay algún hecho del tipo (NumeroPasosTarde), se elimina
; y se añade un nuevo hecho con el contador correcto
; El contador global se resetea a 0
(defrule ResultadoFranjaTarde
        (declare (salience 10))

        ?eliminar1 <- (NumeroPasosTarde ?)
        
        ?eliminar2 <- (ContarHechosMasDesplazamientosFranjaTarde)        

        =>
        
        (retract ?eliminar1)
        (retract ?eliminar2)

        (assert (NumeroPasosTarde ?*n_pasos_franja_tarde*))

        (bind ?*n_pasos_franja_tarde* 0)
)

; Una vez se han contado todos los hechos, si todavía no hay ningún hecho
; (NumeroPasosNoche), significa que es la primera vez que se va a insertar
; un hecho de este tipo, con lo cual no hay que eliminar ningún hecho anterior.
; El contador global se resetea a 0
(defrule ResultadoPrimeroFranjaNoche
        (declare (salience 10))
        
        (not (NumeroPasosNoche ?))

        ?eliminar1 <- (ContarHechosMasDesplazamientosFranjaNoche)        

        =>
        
        (assert (NumeroPasosNoche ?*n_pasos_franja_noche*))
        (retract ?eliminar1)
        (bind ?*n_pasos_franja_noche* 0)
)

; Si hay algún hecho del tipo (NumeroPasosNoche), se elimina
; y se añade un nuevo hecho con el contador correcto
; El contador global se resetea a 0
(defrule ResultadoFranjaNoche
        (declare (salience 10))

        ?eliminar1 <- (NumeroPasosNoche ?)
        
        ?eliminar2 <- (ContarHechosMasDesplazamientosFranjaNoche)        

        =>
        
        (retract ?eliminar1)
        (retract ?eliminar2)

        (assert (NumeroPasosNoche ?*n_pasos_franja_noche*))

        (bind ?*n_pasos_franja_noche* 0)
)

; /*
; *  Deducir si la persona realiza desplazamientos que no hace normalmente por la mañana
; */
(defrule DeducirPersonaMasDesplazamientosFranjaManiana
        (declare (salience -10))

        (NumeroPasosNormalesFranja ?n_pasos franja_maniana)
        (NumeroPasosManiana ?n)
        (test (> ?n (+ ?n_pasos 10)))

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        =>
        (printout t ?h1 ":" ?m1 ":" ?s1 ": La persona realiza más desplazamientos en la franja de maniana" crlf)
)

; /*
; *  Deducir si la persona realiza desplazamientos que no hace normalmente por la tarde
; */
(defrule DeducirPersonaMasDesplazamientosFranjaTarde
        (declare (salience -10))

        (NumeroPasosNormalesFranja ?n_pasos franja_tarde)
        (NumeroPasosTarde ?n)
        (test (> ?n (+ ?n_pasos 10)))

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        =>
        (printout t ?h1 ":" ?m1 ":" ?s1 ": La persona realiza más desplazamientos en la franja de tarde" crlf)
)

; /*
; *  Deducir si la persona realiza desplazamientos que no hace normalmente por la noche
; */
(defrule DeducirPersonaMasDesplazamientosFranjaNoche
        (declare (salience -10))

        (NumeroPasosNormalesFranja ?n_pasos franja_noche)
        (NumeroPasosNoche ?n)
        (test (> ?n (+ ?n_pasos 10)))

        ;; Estos hechos son para una salida en pantalla más organizada
        (hora_actual ?h1)
        (minutos_actual ?m1)
        (segundos_actual ?s1)
        =>
        (printout t ?h1 ":" ?m1 ":" ?s1 ": La persona realiza más desplazamientos en la franja de noche" crlf)
)