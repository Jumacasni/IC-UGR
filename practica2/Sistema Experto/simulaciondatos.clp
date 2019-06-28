(defrule cargardatosasimular
=>
(load-facts DatosSimulados.txt) 
)



(defrule datosensor
(declare (salience 9998))
?f <-  (datosensor  ?h ?m ?s  ?tipo ?habitacion ?val)
(hora_actual ?h1)
(minutos_actual ?m1)
(segundos_actual ?s1)
(test (<= (totalsegundos ?h ?m ?s) (totalsegundos ?h1 ?m1 ?s1)))
=>
(assert (valor ?tipo ?habitacion ?val))
(printout t  ?h1 ":" ?m1 ":" ?s1 ": sensor " ?tipo " " ?habitacion " " ?val crlf)
(retract ?f)
)

(defrule datoluminosidad
(declare (salience 10000))
?f <- (datosensor  ?h ?m ?s  movimiento ?habitacion on)
(luminosidad ?habitacion ?l)
(hora_actual ?h1)
(minutos_actual ?m1)
(segundos_actual ?s1)
(test (<= (totalsegundos ?h ?m ?s) (totalsegundos ?h1 ?m1 ?s1)))
=>
(assert (valor luminosidad ?habitacion ?l))
(printout t  ?h1 ":" ?m1 ":" ?s1 ": sensor luminosidad " ?habitacion " " ?l crlf)
(retract ?f)
)

(defrule datopuerta
(declare (salience 10000))
?f <- (datosensor  ?h ?m ?s  puerta ?habitacion1 ?habitacion2 ?val)
(hora_actual ?h1)
(minutos_actual ?m1)
(segundos_actual ?s1)
(test (<= (totalsegundos ?h ?m ?s) (totalsegundos ?h1 ?m1 ?s1)))
=>
(assert (valor puerta ?habitacion1 ?habitacion2 ?val))
(printout t  ?h1 ":" ?m1 ":" ?s1 ": sensor puerta " ?habitacion1 " " ?habitacion2 " " ?val crlf)
(retract ?f)
)

(defrule datosensor_movimiento_consistencia
(declare (salience 10000))
(datosensor  ?h ?m ?s  movimiento ?habitacion on)
(hora_actual ?h1)
(minutos_actual ?m1)
(segundos_actual ?s1)
(test (<= (totalsegundos ?h ?m ?s) (totalsegundos ?h1 ?m1 ?s1)))
=>
(assert (siguientedatosensor  (hora_suma ?h ?m ?s 60) (minuto_suma ?h ?m ?s 60) (segundo_suma ?h ?m ?s 60) movimiento ?habitacion  on ))
;(printout t "creado siguientedatosensormovimiento "  ?habitacion " para las " (hora_suma ?h ?m ?s 60)   ":" (minuto_suma ?h ?m ?s 60) ":" (segundo_suma ?h ?m ?s 60) crlf)
)


(defrule generar_datosensor_movimiento
(declare (salience 9999))
?f <- (siguientedatosensor  ?h ?m ?s movimiento ?habitacion  on )
(hora_actual ?h1)
(minutos_actual ?m1)
(segundos_actual ?s1)
(test (<= (totalsegundos ?h1 ?m1 ?s1) (totalsegundos ?h ?m ?s)))
=>
(assert (datosensor  ?h ?m ?s movimiento ?habitacion  on ))
(retract ?f)
)

(defrule limpiar_siguientedatosensor_movimiento
(declare (salience 10000))
?f <- (siguientedatosensor  ?h ?m ?s movimiento ?habitacion  on )
(datosensor  ?h1 ?m1 ?s1 movimiento ?habitacion  ?)
(test (<= (totalsegundos ?h1 ?m1 ?s1) (totalsegundos ?h ?m ?s)))
(test (<= (totalsegundos ?h ?m ?s) (+ (totalsegundos ?h1 ?m1 ?s1) 59)))
=>
(retract ?f)
;(printout t "borrado siguientedatosensormovimiento" crlf)
)


(defrule pedirinforme
(declare (salience 10000))
?f <-  (pedirinforme  ?h ?m ?s ?habitacion)
(hora_actual ?h1)
(minutos_actual ?m1)
(segundos_actual ?s1)
(test (>= (totalsegundos ?h1 ?m1 ?s1) (totalsegundos ?h ?m ?s)))
=>
(assert (informe ?habitacion))
(retract ?f)
)