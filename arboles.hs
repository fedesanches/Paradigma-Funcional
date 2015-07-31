-- DEFINICIONES DE TIPO --
type Arbol = ([Char], Double, Double, Double)
type Especie = ([Char], Integer, Double)

-- ESPECIES DE ARBOLES --
-- arbol = (nombre, altura, ancho) para un arbol normal --
pino = ("pino", 3.2, 1.0)
ombu = ("ombu", 4.5, 4.2)
eucalipto = ("eucalipto", 9.0, 1.5)
jacaranda = ("jacaranda", 25.0, 0.7)
cerezo = ("cerezo", 2.5, 0.4)

-- RESERVAS --
-- nombreReserva = listaArboles (nombre, altura, ancho, vitalidad) --
arboles = [ 
		("jacaranda", 6, 1, 1.4),
		("pino", 5, 3, 1.9), 
		("eucalipto", 5, 4, 0.7), 
		("jacaranda", 10, 2, 1.0), 
		("cerezo", 7, 11, 0.9), 
		("ombu", 8, 10, 2.1) ]

laVerde = [
		("jacaranda", 6, 1, 1.4), 
		("pino", 5, 3, 1.9), 
		("eucalipto", 5, 4, 0.7), 
		("jacaranda", 10, 2, 1.0), 
		("cerezo", 7, 11, 0.9), 
		("ombu", 8, 10, 2.1)]

campoLibre = [
		("pino", 5.0, 1.0, 1.2),
        ("pino", 6.0, 0.8, 1.8),
        ("pino", 5.0, 1.1, 1.2),
        ("pino", 5.0, 1.5, 1.1),
        ("pino", 5.0, 0.9, 0.9),
        ("pino", 6.0, 1.1, 1.2),
        ("pino", 5.0, 1.6, 1.0)]


caracteristicasNormales = [pino, ombu, eucalipto, jacaranda, cerezo]

-- PARTE 1 --
-- EJERCICIO A --

esFrondoso (_, altura, ancho, _) = (altura >= 6) && (altura <= 15) && (ancho > altura)

darNombre (nombre, _, _, _) = nombre

nombresFrondosos listaArboles = [darNombre arbol | arbol <- listaArboles, esFrondoso arbol]

-- EJERCICIO B --

buenaVitalidad (_, _, _, vitalidad) = vitalidad > 1

todosLosFrondososSonVitales listaArboles = all buenaVitalidad (filter esFrondoso listaArboles)

-- EJERCICIO C --
-- lluvia aumenta 1 metro la altura y la vitalidad en un porcentaje igual a los milimetros --

lluvia milimetros (nombre, altura, ancho, vitalidad) = (nombre, altura + 1, ancho, vitalidad + (milimetros/100))

-- temperatura, si es bajo cero disminuye la vitalidad a la mitad, si es mas de 40 la disminuye
-- un 40% de lo contrario no afecta

temperatura grados (nombre, altura, ancho, vitalidad)	| grados < 0 = (nombre, altura, ancho, vitalidad/2)
														| grados > 40 = (nombre, altura, ancho, vitalidad * 0.6)
														| otherwise = (nombre, altura, ancho, vitalidad)

-- granizo disminuye a la mitad la altura y el ancho --

granizo (nombre, altura, ancho, vitalidad) = (nombre, altura/2, ancho/2, vitalidad)

-- EJERCICIO D --
hagoPomadaTodo clima listaArboles = map clima listaArboles


-- PARTE 2 --
darAltura (_, altura, _) = altura

darAncho (_, _, ancho) = ancho

coincideNombre nombre (nombreArbol, _, _) = nombreArbol == nombre

-- Devuelve una arbol (tupla) cuyo nombre es igual al dado.
filtroArbol nombre = (head . filter (coincideNombre nombre)) caracteristicasNormales

-- Se ingresa un arbol y devuelve la altura normal de la especie
alturaEspecie = darAltura . filtroArbol

-- Se ingresa un arbol y devuelve el ancho normal de la especie
anchoEspecie = darAncho . filtroArbol

-- EJERCICIO A --

mesSinR mes = not (elem 'r' mes)

sePuedeTransplantar (_, _, _, vitalidad) mes = (mesSinR mes) && (vitalidad > 1)

-- EJERCICIO B -- 

sePuedePodar (nombre, altura, ancho, vitalidad) mes = (sePuedeTransplantar (nombre, altura, ancho, vitalidad) mes) 
							&& (altura > alturaEspecie nombre)

-- EJERCICIO C --

biomasaArbol (_, altura, ancho, vitalidad) = pi * ancho * altura * vitalidad

biomasa listaArboles = sum (map biomasaArbol listaArboles)

-- EJERCICIO D --

gigante (nombre, altura, ancho, vitalidad) = altura > (alturaEspecie nombre) && ancho > (anchoEspecie nombre)

normal (nombre, altura, ancho, vitalidad) = altura == (alturaEspecie nombre) && ancho == (anchoEspecie nombre)

enano (nombre, altura, ancho, vitalidad) = altura < (alturaEspecie nombre) && ancho < (anchoEspecie nombre)

-- EJERCICIO E --

sonTodos funcion listaArboles = all funcion listaArboles

-- EJERCICIO F --

listaPoda reserva mes = [arbol | arbol <- reserva, sePuedePodar arbol mes]

-- EJERCICIO G --

sePuedeMudar reserva mes = all ( flip sePuedeTransplantar mes) reserva 

-- EJERCICIO H --

biomasaPoda listaArboles mes = 0.75 * ((biomasa .listaPoda listaArboles) mes)