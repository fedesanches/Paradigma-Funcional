type Cadena = [Char]
type Pollo = (Cadena, Integer, Integer, [Cadena])

-- pollo (nombre, diasAstronomicos, peso, arteMarcial) --
ginger = ("ginger",8968410, 150, ["kung fu", "tai chi chuan"])
rocky = ("rocky",8968300, 300, ["karate"])
anunaki = ("anunaki",8968888, 340, ["karate","muay thay","taekwondo"])

listaPollos = [ginger, rocky, anunaki]

darPeso (_, _, peso, _) = peso
darNombre (nombre, _, _, _) = nombre
darDias (_, dias, _, _) = fromIntegral dias -- pasar integer a double
darListaTecnicas (_, _, _, lista) = lista

-- PARTE 1 --
-- EJERCICIO 1 --

edad pollo = (darDias pollo) / (pi * 365 * 365)

-- EJERCICIO 2 --

esAdulto pollo = (edad pollo) > 5

-- EJERCICIO 3 --

esJoven pollo = (edad pollo) < 5

-- EJERCICIO 4 --

sinFondo pollo = even ( length (darNombre pollo))

estaDesnutrido pollo | (esJoven pollo) && (darPeso pollo < 50) = True
					 | (esAdulto pollo) && (darPeso pollo < 200) = True
					 | (sinFondo pollo) = True
					 | otherwise = False

-- EJERCICIO 5 --

engordar cantidad (nombre, dias, peso, arteMarcial) = (nombre, dias, peso + cantidad, arteMarcial)

-- EJERCICIO 6 --

alimentar :: Integer -> Pollo -> Pollo
alimentar cantidad pollo | estaDesnutrido pollo = engordar cantidad pollo
						 | esAdulto pollo = engordar (div cantidad 2) pollo
						 | otherwise = pollo  

-- PARTE 2 --
-- EJERCICIO 1 --
noSabe arte (nombre, dias, peso, arteMarcial) = not(elem arte arteMarcial)
aprende arte (nombre, dias, peso, arteMarcial) = (nombre, dias, peso, arteMarcial ++ [arte])
agregarNombre frase (nombre, dias, peso, arteMarcial) = (frase ++ nombre, dias, peso, arteMarcial)

arguiniano pollo = engordar 100 pollo

miyagi pollo | noSabe arte pollo = aprende arte pollo
			 | otherwise = pollo 

marcelito (nombre, dias, peso, arteMarcial) = (nombre, dias, peso, [])

brujaTapita	(alturaRaton, pesoRaton, bigotes) pollo = alimentar ((alturaRaton * pesoRaton) - bigotes) pollo

marioBross arte pollo | noSabe arte pollo = ((agregarNombre "super mario").(aprende arte).(aprende "salto")) pollo		
					  | otherwise = ((agregarNombre "super mario").(aprende "salto")) pollo

marcenano pollo = (arguiniano . marcelito) pollo 					 