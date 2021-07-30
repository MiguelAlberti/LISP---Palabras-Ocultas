;------------------------------------------------------------------
;PINTA UNA IMAGEN A COLOR
;------------------------------------------------------------------
(defun pintarColor (nombre coordx coordy longitud) 
    (setq fichero (open nombre :direction :input
        :element-type 'unsigned-byte))
    (setq pixel 1)
    (setq x coordx)
    (setq y coordy)
    (setq R 0 G 0 B 0)
    (move x y)
    (loop
        (setq B (read-byte fichero nil) G (read-byte fichero nil) 
            R (read-byte fichero nil))
        (if (or (null R) (null G) (null B)) (return ()))
        (color R G B)
        (draw (+ 1 x) y)
        (setq pixel (+ pixel 1))
        (setq x (+ x 1))
        (cond ((> pixel longitud) (setq pixel 1) 
            (setq x coordx) (setq y (+ y 1))))
        (move x y)
    )
    (close fichero)
    (color 0 0 0)
)

;------------------------------------------------------------------
;FUNCION QUE PINTA UN RECTANGULO CON LOS PARAMETROS QUE SE INDIQUEN
;------------------------------------------------------------------
(defun pintaCuadrado (x1 y1 x2 y2)
    (move x1 y1)
    (draw x2 y1 x2 y2 x1 y2 x1 y1)
)

;------------------------------------------------------------------
;FUNCION QUE PINTA LA SECCION DE LA IMAGEN DE ARRIBA-DERECHA
;------------------------------------------------------------------
(defun cuadroImagen ()
    (pintaCuadrado 438 171 638 373)
    (pintaCuadrado 437 170 639 374)
    (setq valorx 437 valory 170)
    (dotimes (i 203) ;para ocultar la imagen, pintar toda de negro
        (move valorx valory)
        (color 0 0 0)
        (draw 639 valory)
        (setq valory (+ valory 1))
    )
)

;------------------------------------------------------------------
;FUNCION QUE PINTA LA SECCION DEL TITULO
;------------------------------------------------------------------
(defun titulo ()
    (pintaCuadrado 1 269 434 373)
    (pintaCuadrado 0 268 435 374)
    (setq valorx 0 valory 270)
    (dotimes (i 105)
        (move valorx valory)
        (color 0 0 0)
        (draw 434 valory)
        (setq valory (+ valory 1))
    )
    (setq valorx 5 valory 323) 
    (dotimes (i (length "PALABRAS"))
        (pintarColor (strcat "img/" (string (char "PALABRAS" i)) ".img") valorx valory 52)  
        (setq valorx (+ valorx 53))
    )
    (setq valorx 5 valory 270) 
    (dotimes (i (length "OCULTAS"))
        (pintarColor (strcat "img/" (string (char "OCULTAS" i)) ".img") valorx valory 52)  
        (setq valorx (+ valorx 53))
    )
)

;------------------------------------------------------------------
;FUNCION QUE PINTA LA SECCION DE LOS DATOS COMO JUGADOR, PUNTOS Y JUGADAS
;------------------------------------------------------------------
(defun datosPartida ()
    (pintaCuadrado 1 171 434 200)
    (pintaCuadrado 0 170 435 201)
    (pintaCuadrado 1 202 434 232)
    (pintaCuadrado 0 201 435 233)
    (pintaCuadrado 1 232 434 265)
    (pintaCuadrado 0 235 435 266)
    (setq valorx 10 valory 175)
    (dotimes (j 3) 
        (dotimes (i 18)   
            (pintaCuadrado valorx valory (+ valorx 21) (+ valory 21))
            (setq valorx (+ valorx 23))
        )
        (setq valorx 10 valory (+ valory 32))
    )
    (setq valorx 11 valory 240) ;pone las letras de la palabra jugador 
    (dotimes (i (length "JUGADOR"))
        (pintarColor (strcat "img/" (string (char "JUGADOR" i)) "_BN.img") valorx valory 20)  
        (setq valorx (+ valorx 23))
    )
    (setq valorx 11 valory 208) ;pone las letras de la palabra puntos 
    (dotimes (i (length "PUNTOS"))
        (pintarColor (strcat "img/" (string (char "PUNTOS" i)) "_BN.img") valorx valory 20)  
        (setq valorx (+ valorx 23))
    )
    (setq valorx 11 valory 176) ;pone las letras de la palabra jugadas 
    (dotimes (i (length "JUGADAS"))
        (pintarColor (strcat "img/" (string (char "JUGADAS" i)) "_BN.img") valorx valory 20)  
        (setq valorx (+ valorx 23))
    )
)

;------------------------------------------------------------------
;PINTA LOS CUADROS DE UNA PALABRA QUE TENGA UN NUMERO DE LETRAS PAR
;------------------------------------------------------------------
(defun palabrapar (longitud)
    (setq valorx 326 valory 100)
    (dotimes (i (/ longitud 2))   
        (pintaCuadrado valorx valory (+ valorx 53) (+ valory 53))
        (setq valorx (+ valorx 60))
    )     
    (setq valorx 266 valory 100)
    (dotimes (i (/ longitud 2))   
        (pintaCuadrado valorx valory (+ valorx 53) (+ valory 53))
        (setq valorx (- valorx 60))
    )
)

;------------------------------------------------------------------
;PINTA LOS CUADROS DE UNA PALABRA QUE TENGA UN NUMERO DE LETRAS IMPAR
;------------------------------------------------------------------
(defun palabraimpar (longitud)
    (setq valorx 295 valory 100)
    (pintaCuadrado valorx valory (+ valorx 53) (+ valory 53))
    (setq valorx 355 valory 100)
    (dotimes (i (/ (1- longitud) 2))   
        (pintaCuadrado valorx valory (+ valorx 53) (+ valory 53))
        (setq valorx (+ valorx 60))
    )     
    (setq valorx 235 valory 100)
    (dotimes (i (/ (1- longitud) 2))   
        (pintaCuadrado valorx valory (+ valorx 53) (+ valory 53))
        (setq valorx (- valorx 60))
    )
)

;------------------------------------------------------------------
;FUNCION QUE PINTA LA SECCION DE LOS CUADROS EN BLANCO DE LAS LETRAS DE UNA PALABRA
;------------------------------------------------------------------
(defun cuadroPalabra (palabra)
    (setq long (length palabra))
    (pintaCuadrado 1 92 638 167)
    (pintaCuadrado 0 93 639 168)
    (cond
     ((= (mod long 2) 1) (palabraimpar long))
     (t (palabrapar long))
    )
)
 
;------------------------------------------------------------------
;FUNCION QUE PINTA LA SECCION DE LA VENTANA DE COMUNICACION
;------------------------------------------------------------------
(defun ventanaComunicacion ()
    (pintaCuadrado 1 1 638 40)
    (pintaCuadrado 0 0 639 41)
)

;------------------------------------------------------------------
;FUNCION QUE PINTA LA SECCION DE PALABRAS FALLADAS
;------------------------------------------------------------------
(defun letrasFalladas ()
    (pintaCuadrado 1 44 638 88)
    (pintaCuadrado 0 43 639 89)

    (pintarColor 'img/letrasfalladas.img 20 45 176)

    (setq valorx 220 valory 54)
    (dotimes (i 17)   
            (pintaCuadrado valorx valory (+ valorx 21) (+ valory 21))
            (setq valorx (+ valorx 24))
        )
)

;------------------------------------------------------------------
;FUNCION QUE LLAMA A TODAS LAS SECCIONES DE LA INTERFICIE PARA PINTARLAS
;------------------------------------------------------------------
(defun imprimirPantalla ()
    (titulo)
    (datosPartida)
    (pintaCuadrado 1 93 638 167)
    (pintaCuadrado 0 92 639 168)
    (letrasFalladas)
    (ventanaComunicacion)
    (cuadroImagen)
)

;------------------------------------------------------------------
;FUNCION QUE INSERTA UN DATO YA SEA EL NOMBRE DE USUARIO, PUNTOS O LAS JUGADAS
;------------------------------------------------------------------
(defun ponerDato (dato posx posy)
    (setq valorx posx valory posy)
    (dotimes (i (length dato))
        (pintarColor (strcat "img/" (string (aref dato (- (- (length dato) 1) i)))
             "_NB.img") valorx valory 20)
        (setq valorx (- valorx 23))
    )
)

;------------------------------------------------------------------
;FUNCION QUE GUARDA EL RESULTADO DE LA PARTIDA EN EL FICHERO AL ACABAR
;------------------------------------------------------------------
(defun guardarResultadosPartida (nombre puntuacion)
    (setq ficheroRes (open "resultados.txt" :direction :output :if-exists :append :if-does-not-exist :create))
    (setq datos (strcat nombre ": " puntuacion))
    (print datos ficheroRes)
    (close ficheroRes)
)

;------------------------------------------------------------------
; Metodo en el cual metemos las palabras del fichero dentro de Palabra
;------------------------------------------------------------------
(defun jugar()
    (setq f (open "palabras.txt" :direction :input))
    (loop 
        (setq w (read-line f nil)) ;lee una linea del fichero de palabras
        (if (null w) (return())) ;si no hay mas lineas con palabras a leer saldrá
        (setq listapalabra '()) ;lista que contendrá las letras una por una
        (dotimes (i (length w))
            (setq listapalabra (append listapalabra (list (aref w i)))) ;se hace una lista con las letras para poder saber cuando ha puesto todas
        )
        (borrar 0 639 28 168) ;borra unicamente las secciones de letra falladas y el cuadro de letras de la palabra a acertar
        (cuadroImagen) 
        (letrasFalladas)  ;vuelve a pintar las secciones borradas
        (cuadroPalabra w)
        (setq erroneas 0) ;variable que contendrá las veces que se ha equivocado
        (setq listaletrasfalladas '())
        (setq intentos '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
        (initImagenGrande w)  
        (loop 
            (loop
                (imprimirLinia "Introduce una letra: ")
                (setq teclado (string-downcase (read))) ;lectura por teclado de la letra
                (if (= (length teclado) 1) (return ()))
            
             )  
            (setq trobat "no") ;variable para ver si se ha encontrado coincidencia de la letra introducida con alguna de la palabra
            (dotimes (i (length w))
                (setq letra (string (aref w i)))
                (if (string= letra teclado) (setq trobat "si"))
                (if (string= letra teclado) (ponerletracorrecta (length w) teclado i)) ;pinta la letra correcta
                (if (string= letra teclado) (setq listapalabra (remove (coerce teclado 'character) listapalabra))) ;elimina la letra de la lista de letras
            )
            (if (not (string= trobat "si")) (ponerletrafallada teclado)) ;pinta la letra erronea
            (if (= erroneas 17) (imprimirLinia "Ha agotado todas sus posibilidades. La solucion es la siguiente."))
            (if (= erroneas 17) (return ())) ;ha agotado todas sus posibilidades
            (if (null listapalabra) (imprimirLinia (strcat "Ha completado correctamente la palabra. Su puntuacion es: " (format nil "~a" (- 255 (* erroneas 15))))))
            (if (null listapalabra) (return ())) ;si la lista de letras esta vacia es que se han puesto todas y se ha adivinado la palabra
        )
        (solucionar w)
        (setq totalpuntos (+ totalpuntos (- 255 (* erroneas 15)))) ;realiza la suma de puntos
        (setq totaljugadas (1+ totaljugadas)) ;suma la jugada
        (dotimes (i 3000000))
        (imprimirLinia "Quiere volver a jugar? Indique 'si' o 'no'. ") ;no se ha puesto el primer interrogante porque salia un error con un simbolo raro
        (loop
            (setq respuesta (string (read)))
            (cond
                ((string= respuesta "NO") (return ()))
                ((string= respuesta "SI") (return ())) 
                (t (imprimirLinia "Introduzca un valor correcto: "))
            )
        )  
        (ponerDato (format nil "~a" totaljugadas) 402 176) ;coloca el numero de jugadas
        (ponerDato (format nil "~a" totalpuntos) 402 208) ;coloca los puntos totales
        (if (string= respuesta "NO") (return ()))   
    )
    (if (null w) (imprimirLinia "No quedan mas palabras para adivinar. El programa finalizara."))
    (close f)
)

;------------------------------------------------------------------
; Borra una seccion determinada, en realidad la pinta toda de blanco
;------------------------------------------------------------------
(defun borrar (x1 x2 y1 y2)
    (color 255 255 255)
    (dotimes (i (- y2 y1))
        (move x1 (+ y1 i))
        (draw x2 (+ y1 i))
    )
    (color 0 0 0)
)

;------------------------------------------------------------------
; Completa la palabra e imagen al llegar a los 17 errores y no la ha adivinado
;------------------------------------------------------------------
(defun solucionar (word)
    (dotimes (i (length word))
        (setq letra (string (aref w i)))
        (ponerletracorrecta (length w) letra i)
    )
    (pintarColor (strcat "img/" w ".img") 439 172 200)
)

;------------------------------------------------------------------
; Pone la imagen de una letra al adivinarla
;------------------------------------------------------------------
(defun ponerletracorrecta (longitud letra posicion)
    (if (< posicion (/ longitud 2)) 
        (setq valorx (- 266 (* (1- (- (/ longitud 2) posicion)) 60)) valory 101)
        (setq valorx (+ 326 (* (- posicion (/ longitud 2)) 60)) valory 101))
    (if (= (mod longitud 2) 0) (setq valorx (+ valorx 1)))    
    (pintarColor (strcat "img/" letra ".img") valorx valory 52)
    
)

;------------------------------------------------------------------
; Método encargado de inicializar la imagen grande
;------------------------------------------------------------------
(defun initImagenGrande (w)
    (setq ficheroimg (open (strcat "img/" w ".img") :direction :input
        :element-type 'unsigned-byte))
    (setq arraypixelesimg (make-array 200))
    (dotimes (i 200)
		(setf (aref arraypixelesimg i) (make-array 200))
        (dotimes (j 200)
		    (setf (aref (aref arraypixelesimg i) j) (make-array 3))
        ) 
    ) 

    (dotimes (i 200)
		(dotimes (j 200)
            (dotimes (k 3)
                (setq b (read-byte ficheroimg nil))
			    (setf (aref (aref (aref arraypixelesimg i) j) k) b)
            )
        )
    )    
    (close ficheroimg)
)

;------------------------------------------------------------------
; Método encargado de descubrir la imagen al fallar
;------------------------------------------------------------------
(defun colocarSeccion ()
    (setq numsec (random (length intentos)))
    (setq seccion (nth numsec intentos))
    (setq intentos (remove seccion intentos))
    (setq pixel 1)
    (setq x (* 50 (- 3 (mod seccion 4))))
    (cond
        ((< seccion 4) (setq y 0))
        ((< (- seccion 4) 4) (setq y 50))
        ((< (- seccion 8) 4) (setq y 100))
        ((<= (- seccion 12) 4) (setq y 150))
    )
    (setq coordx (+ 439 x) coordy (+ 172 y))
    (setq x (+ 439 x) y (+ 172 y))
    (setq R 0 G 0 B 0)
    (move x y)
    (loop
        (if (= y (+ coordy 50)) (return ()))
        (setq B (aref (aref (aref arraypixelesimg (- y 172)) (- x 439)) 0))
        (setq G (aref (aref (aref arraypixelesimg (- y 172)) (- x 439)) 1))
        (setq R (aref (aref (aref arraypixelesimg (- y 172)) (- x 439)) 2))
   
        (color R G B)
        (draw (+ 1 x) y)
        (setq pixel (+ pixel 1))
        (setq x (+ x 1))
        (cond ((> pixel 50) (setq pixel 1) 
            (setq x coordx) (setq y (+ y 1))))
        (move x y)
    )
    
    (color 0 0 0)
)

;------------------------------------------------------------------
; Pone la imagen de una letra erronea
;------------------------------------------------------------------
(defun ponerletrafallada (letra) 
    (setq estacolocada 0)
    (dotimes (j (length listaletrasfalladas))
        (if (string= (string (elt listaletrasfalladas j)) letra) (setq estacolocada 1))
    )
    (if (= estacolocada 0) (setq erroneas (+ erroneas 1)))
    (if (= estacolocada 0) (pintarColor (strcat "img/" letra "_NB.img")
     (+ 221 (* (1- erroneas) 24)) 55 20))
    (if (= estacolocada 0) (setq listaletrasfalladas (append listaletrasfalladas (list letra))))
    (if (and (/= erroneas 17) (= estacolocada 0)) (colocarSeccion))
)

;------------------------------------------------------------------
; Mantiene la linia de la ventana de comunicacion siempre en el mismo lugar
;------------------------------------------------------------------
(defun imprimirLinia (texto) 
    (goto-xy 10 23)
    (cleol)
    (ventanaComunicacion)
    (princ texto)
)
;------------------------------------------------------------------
; FUNCION INICIO
;------------------------------------------------------------------
(defun inicio ()
    (cls)
    (setq nombreusuario "") ;variable con el nombre del usuario
    (setq totaljugadas 0) ;valor que contará las jugadas de la partida
    (setq totalpuntos 0) ;valor que contará los puntos de la partida
    (imprimirPantalla)
    (imprimirLinia "Introduzca su nombre: ")
    (setq nombreusuario (string (read)))
    (ponerDato nombreusuario 402 240)
    (ponerDato (format nil "~a" totaljugadas) 402 176) ;coloca el numero de jugadas
    (ponerDato (format nil "~a" totalpuntos) 402 208) ;coloca los puntos totales
    (jugar)
    (guardarResultadosPartida nombreusuario (format nil "~a" totalpuntos))
     
)
