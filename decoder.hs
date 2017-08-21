-- Practica Programacion declarativa (descifrador de textos)
-- Asier Cardoso Sánchez


{-EXPLICACIÓN DE LA PRÁCTICA

Para ejecutar la practica, se debe llamar a la función programa.
Lo primero que hace está función es calcular la frecuencia de letras
que tiene "Quijote.txt", muestra las frecuencias y nos pide el nombre
del fichero codificado (por defecto "texto.cod").

Una vez introducido, se hace el analisis de frecuencia de letras
del texto codificado. Teniendo las dos frecuencias, se hace una 
biyeccion entre ellas.

Después se muestra un menu para descifrar manualmente o automaticamente
(La opcion automatica no esta implementada así que solo es posible
hacerlo de forma manual)

Después en otro menu podemos decidir si queremos ir
cambiando letra por letra o por palabras.

En la opción de letra por letra , nos muestra una traducion parcial
a partir de la biyeccion inicial que hemos sacado automaticamente y
nos pide dos letras (formato "ab", donde modificara la biyeccion para
que las apariciones que habia de a's en el texto ahora sen b's)

En la opción de palabras, se va recorriendo el texto palabra por palabra,
mostrando una lista de palabras por las que se puede cambiar (con intro
se deja la misma palabra, y con un número se cambia por la 
correspondiente en la lista)

Tras cualquiera de las dos opciones.se sale y se genera el fichero
"d_texto.cod" o "d_NOMBRE_FICHERO" si se uso un nombre distinto.
-}


--Tipos que nos pueden ser de utilidad
type Biyeccion = [(Char, Char)]
type Frecuencia = [(Int, Char)]
type FrecuenciaPalabras = [(Int, String)]
type BiyeccionPalabras = [(String, String)]--BORRAR

abecedario = ['a'..'z']	
caracteresRaros = "çâêîôû\'ñàèìòù" --Los textos codificados no tienen ñ

rutaQuijote = "Quijote.txt"
rutaCifrado = "texto.cod" --Fichero de entrada por defecto
rutaPalabrasCastellano = "RAE.txt" --Palabras del castellano dado

--Función principal
programa :: IO ()
programa = do
  quijote <- readFile rutaQuijote
  let fQuijote = frecuencias quijote
  putStr("Analizando el Quijote")
  print(fQuijote)
  putStr("Introduzca el nombre del fichero cifrado: ")
  (cifrado, rutaSalida) <- obtenerTexto rutaCifrado
  let fCifrado = frecuencias cifrado
  putStr("Se analizo cifrado") 
  print(fCifrado) 
  let descifrador = deduceBiyeccion fCifrado fQuijote
  putStr("Biyeccion: ")
  print(descifrador)
  descifrador <- menuDescifrador cifrado descifrador
  print(descifrador)
  let textoDescifrado = traduccion descifrador cifrado
  writeFile rutaSalida textoDescifrado
  putStr("Se creo el fichero de salida " ++ rutaSalida)
 
{-Recibe el nombre de fichero por defecto donde esta el texto
codificado y devuelve el texto y el nombre por defecto del archivo
donde se escribira el texto traducido -} 
obtenerTexto::String -> IO (String, String)	
obtenerTexto rutaDefecto = do
  rutaEntrada <- getLine
  if rutaEntrada == "" then do
                               texto <- readFile rutaDefecto
                               return (texto, "d_" ++ rutaDefecto)
                         else do
						       texto <- readFile rutaEntrada
						       return (texto, "d_" ++ rutaEntrada)

--Escribe la traducion en el fichero de salida
escribirTexto :: String -> FilePath -> IO ()						 
escribirTexto texto rutaDefecto =  do
  rutaSalida <- getLine
  if rutaSalida == "" then do 
                              writeFile rutaDefecto (texto)
					  else do 
					          writeFile rutaSalida (texto)								   

--Menu para seleccionar auto o manual							  
menuDescifrador::String -> Biyeccion -> IO Biyeccion
menuDescifrador texto biy =  do
    putStr("Que prefieres: \n1.Descifrar automaticamente \n2.Descifrar manualmente\n")
    op <- getLine
    case op of 
       "1" -> menuManual texto biy
       --"1" -> menuAutomatico texto biy
       "2" -> menuManual texto biy
       _   -> menuDescifrador texto biy
	   
	   
--Menu para seleccionar modo manual							  
menuManual::String -> Biyeccion -> IO Biyeccion
menuManual texto biy =  do
    putStr("Que prefieres: \n1.Cambiar letra \n2.Cambiar palabra\n")
    op <- getLine
    case op of 
       "1" -> manualLetra texto biy
       --"2" -> manualLetra texto biy
       "2" -> manualPalabra texto biy
       _   -> menuManual texto biy

	   
--------------- 
--MODO MANUAL--
---------------
manualLetra::String->Biyeccion-> IO Biyeccion
manualLetra texto biy = do
   putStr("Biyeccion: ")
   biy <- cambiaBiyeccionLetra biy texto
   return biy

cambiaBiyeccionLetra:: Biyeccion -> String -> IO Biyeccion
cambiaBiyeccionLetra biy cifrado = do
  print(biy)
  let textoDescifrado = traduccion biy cifrado
  putStr(textoDescifrado)
  putStr("Introduzca dos letras, donde la segunda sea el valor por el cual quieres sustituir la primera en las apariciones del texto:\n")
  f <- getChar
  if f /= '\n' then do
      f' <- getChar
      c <- getChar
      cambiaBiyeccionLetra (cambia f f' biy) cifrado
              else return biy  
   
manualPalabra::String->Biyeccion-> IO Biyeccion
manualPalabra texto biy = do
   dicc <- palabrasCastellano 50000
   let palabras = map (filter esLetra) (words (map aMinuscula texto))
   biy <- cambiaBiyeccionPalabra biy texto palabras dicc
   return biy   
   
cambiaBiyeccionPalabra:: Biyeccion -> String -> [String] -> [String] -> IO Biyeccion
cambiaBiyeccionPalabra biy cifrado [] dicc = return biy
cambiaBiyeccionPalabra biy cifrado (p:ps) dicc = do
  putStr("Biyeccion: ")
  print(biy)
  let textoDescifrado = traduccion biy cifrado
  putStr(textoDescifrado)
  let p' = traduccion biy p
  let posibles = posiblesTraducciones p dicc
  putStr(p'++": ")
  print(take 20(zip [0..] posibles)) --Muestra las palabras por las que se puede traducir
  f <- getLine
  if f /= "" && f /= "s" then do
      let palabra' = posibles !! (read f::Int)
      let biy' = fuerzaTraduccionPalabra p palabra' biy
      cambiaBiyeccionPalabra biy' cifrado ps dicc
              else if f == "" then cambiaBiyeccionPalabra biy cifrado ps dicc
			                      else return biy     

--Dadas dos palabras y una biyeccion se cambia la biyeccion para que traduzca una palabra por la otra								  
fuerzaTraduccionPalabra:: [Char] -> [Char] -> Biyeccion -> Biyeccion								  
fuerzaTraduccionPalabra [] p' biy = biy
fuerzaTraduccionPalabra p [] biy = biy  
fuerzaTraduccionPalabra (p:ps) (p':ps') biy = fuerzaTraduccionPalabra ps ps' (fuerzaTraduccion p p' biy)

-------------------   
--MODO AUTOMATICO--
-------------------


--esSolucion:: [String] -> Bool
esSolucion []  dicc =  True
esSolucion (p:ps) dicc = (estaEnDiccionario p dicc)&&(esSolucion ps dicc)               

estaEnDiccionario:: String -> [String] -> Bool
estaEnDiccionario p dicc = elem p dicc

	   

parejaDiferentes:: String -> String -> (Char, Char)
parejaDiferentes (x:xs) (y:ys) = if x == y then parejaDiferentes xs ys
                                      else (x,y)	   
	   
diferencias:: String -> String -> Int
diferencias [] y = 0
diferencias x [] = 0
diferencias (x:xs) (y:ys) = if x == y then diferencias xs ys
                                      else 1 + diferencias xs ys	   

añadeLetra l letras = if elem l letras then letras
                                       else letras ++ l:[]
									   
quitaLetra letras l = if elem l letras then quitaLetra' letras l
                                       else letras

quitaLetra' [] l = []									   
quitaLetra' (x:xs) l = if l == x then quitaLetra' xs l
                                 else (x:quitaLetra' xs l)
								 
								 
								 
								 
 
---------------------------
--FUNCIONES AUXILIARES--
---------------------------

--quick sort de mayor a menor
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  biggerSorted ++ [x] ++ smallerSorted
	
esLetra::Char -> Bool
esLetra l = esMinuscula l || esMayuscula l
esMinuscula l = l >= 'a' && l <= 'z'
esMayuscula l = l >= 'A' && l <= 'Z' 


dif = fromEnum 'a' - fromEnum 'A'

aMinuscula::Char -> Char
aMinuscula l = if esMayuscula l then toEnum (fromEnum l + dif)::Char
                              else l
aMayuscula l = if esMinuscula l then toEnum (fromEnum l - dif)::Char
                              else l							  

mismaLetra::Char -> Char -> Bool
mismaLetra l l' = (aMinuscula l == aMinuscula l')
distintaLetra l l' = not (mismaLetra l l')

-------------------------
--FUNCIONES PRINCIPALES--
-------------------------

-------------------
--ANALISIS LETRAS--
-------------------

-- frecuencias ordenadas de un texto en minuscula con el abecedario usual
frecuencias:: String -> Frecuencia	
frecuencias f = quicksort( frecuencias' (map aMinuscula f) abecedario)

--frecuencias de las letras de un abecedario en un texto dado
frecuencias':: String -> String -> Frecuencia
frecuencias' [] (a:a') = (0, a):(frecuencias' [] a')
frecuencias' x [] = []
frecuencias' (x:xs) abc 
  | elem x abc = let repeticiones = length(filter (== x) (x:xs)) in (repeticiones, x):(frecuencias' (filter (/= x) xs) (filter (/= x) abc))
  | otherwise = frecuencias' (filter (/= x) xs) abc

	
inversa::Biyeccion -> Biyeccion
inversa [] = []
inversa ((a,a'):resto) = ((a',a):inversa resto)


traduceCaracter :: Eq a1 => [(a1, a)] -> a1 -> a
traduceCaracter biy l = head (map snd (filter (\(x,x') -> x == l) biy))

--Devuelve el texto traducido, dado un texto y una biyeccion 
traduccion:: Biyeccion -> String -> String
traduccion b [] = []
traduccion b (l:ls) 
    | esMinuscula l = traduceCaracter b l:(traduccion b ls)
	| esMayuscula l = aMayuscula(traduceCaracter b (aMinuscula l)):(traduccion b ls)
	| otherwise = l:(traduccion b ls)


--Empareja dos frecuencias generando una biyeccion
deduceBiyeccion :: [(a1, a)] -> [(a2, b)] -> [(a, b)]
deduceBiyeccion [] b' = []
deduceBiyeccion b [] = []
deduceBiyeccion b b' = zip (map snd b) (map snd b')

--La funcion fuerzaTraduccion, se encarga de dada una biyeccion de forzar 
--que la letra i se traduzca por la letra f. Emparejando consecuentemente
--las dos letras que quedan sueltas-}
--             i     a   b    pf            i    a    b    f
--             |    |    |    |  =>       |    |    |    |
--             pi    a'  b'    f            f    a'    b'   pi
fuerzaTraduccion:: Char -> Char -> Biyeccion -> Biyeccion
fuerzaTraduccion i f biy =
  let pi = parejaInicial i biy 
      pf = parejaFinal f biy
  in  empareja i f pf pi biy

--calcula una nueva biyeccion dando un par de caracteres que quieren que
--se intercambien en el texto
--           pf    a    b    pf'          pf    a    b   pf'
--             |    |    |    |  =>       |    |    |    |
--            f    a'    b'    f'            f'   a'    b'   f 
cambia::Char -> Char -> Biyeccion -> Biyeccion  
cambia f f' biy
 | f == f' = biy
 | otherwise = fuerzaTraduccion (parejaFinal f biy) f' biy 
  
   
-- nos dice con quien esta emparejado i, siendo i la primera letra en la pareja. (i,pi)  
parejaInicial:: Char -> Biyeccion -> Char
parejaInicial i [] = ' '
parejaInicial i ((a,a'):bs) 
  | a == i = a'
  | otherwise = parejaInicial i bs
  
parejaFinal:: Char -> Biyeccion -> Char
parejaFinal f [] = ' '
parejaFinal f ((a,a'):bs) 
  | a' == f = a
  | otherwise = parejaFinal f bs  

--             i     a   b    pf            i    a    b    f
--             |    |    |    |  =>       |    |    |    |
--             pi    a'  b'    f            f    a'    b'   pi
empareja:: Char -> Char -> Char -> Char -> Biyeccion -> Biyeccion
empareja i f pf pi [] = []
empareja i f pf pi ((b,b'):bs) 
  |	(i == pf && f == pi) = ((b,b'):bs) 
  | i == b = ((i,f):(pf,pi):empareja i f pf pi (filter (/= (pf,f)) bs))
  | f == b' = ((i,f):(pf,pi):empareja i f pf pi (filter (/= (i,pi)) bs))
  | otherwise = ((b,b'):(empareja i f pf pi bs))


---------------------
--ANALISIS PALABRAS--
---------------------

--se da por hecho que el texto viene en minuculas, si no "Casa" y "casa" serian distintas palabras 
frecuenciaPalabras:: String -> [String]
frecuenciaPalabras [] = []
frecuenciaPalabras texto = do                                   
   let palabras = map (filter esLetra) (words (map aMinuscula texto)) in map snd (quicksort (frecuenciaPalabras' palabras))
   
   
frecuenciaPalabras'::[String] ->  FrecuenciaPalabras
frecuenciaPalabras' [] = []
frecuenciaPalabras' (p:ps) = let repeticiones = length(filter (p ==) (p:ps)) in ((repeticiones,p):(frecuenciaPalabras' (filter (p /=) ps)))
   									  
patron :: (Num t1, Eq t) => [t] -> [t1]									  
patron palabra = patron' palabra [] 0

patron' :: (Num t1, Eq t) => [t] -> [(t, t1)] -> t1 -> [t1]
patron' [] ptr i = []
patron' (p:ps) ptr i
     | elem p (map fst ptr) = ((traduceCaracter ptr p):(patron' ps ptr i))
     | otherwise = do let ptr' = ((p, i):ptr) in (i:patron' ps ptr' (i+1))


posiblesTraducciones:: String -> [String] -> [String]	
posiblesTraducciones palabra [] = []
posiblesTraducciones palabra (d:ds) 
     |  patron palabra == patron d = d:posiblesTraducciones palabra ds
     | otherwise = posiblesTraducciones palabra ds	 
   
------------------------------------------------
--PALABRAS CASTELLANO ORDENADAS POR FRECUENCIA--   
------------------------------------------------

{-
Con ayuda de estas funciones, se saca la lista de palabras
del castellano a partir de RAE.txt
-}
palabrasCastellano :: (Num a, Eq a) => a -> IO [String]
palabrasCastellano x = do 
   f <- readFile rutaPalabrasCastellano
   let g = destilda(consumeLineas f 5)
   return(filter palabraNormal (guardaPalabras g x))   

--Llamar con n>1
consumeLineas :: (Num a, Eq a) => [Char] -> a -> [Char]
consumeLineas f n = do 
           let f' = dropWhile (/= '\n') f
           if n == 1 then tail f'
                     else consumeLineas (tail f') (n-1)
--Llamar con n>1
guardaPalabras :: (Num a, Eq a) => [Char] -> a -> [[Char]] 
guardaPalabras f n = do 
           let f' = dropWhile (not.esLetra) f
           let palabra = takeWhile (/= ' ') f'
           let g = dropWhile esLetra f'
           if n == 1 then (palabra:[])
                     else (palabra:guardaPalabras g (n-1))	   
					 
destilda:: String -> String
destilda [] = []
destilda (p:ps) = (destildaLetra p:destilda ps) 

destildaLetra:: Char -> Char
destildaLetra l 
    | l == 'á' = 'a'
    | l == 'é' = 'e'
    | l == 'í' = 'i'
    | l == 'ó' = 'o'
    | l == 'ú' = 'u'
    | l == 'ü' = 'u'
    | otherwise = l

palabraNormal:: String -> Bool
palabraNormal [] = True
palabraNormal (s:ss) = not(elem s caracteresRaros) && palabraNormal ss