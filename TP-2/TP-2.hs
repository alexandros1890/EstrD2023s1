--1.1
sumatoria :: [Int] -> Int
sumatoria  [] = 0
sumatoria (n:ns) = n + sumatoria ns

--1.2
longitud :: [a] -> Int
longitud [] = 0
longitud (a:as) = 1 + longitud as

--1.3
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = x+1 : sucesores xs

--1.4
conjuncion :: [Bool] -> Bool
conjuncion [] = False
conjuncion (x:xs) = x && conjuncion xs

--1.5
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

--1.6
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (a:as) = a ++ aplanar as

--1.7
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = e==x || pertenece e xs 

--1.8
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = if e==x then 1 + apariciones e xs else apariciones e xs

--1.9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (x:xs) = if n > x then x : losMenoresA n xs else losMenoresA n xs

--1.10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x:xs) = if n < longitud x then x : lasDeLongitudMayorA n xs else lasDeLongitudMayorA n xs

--1.11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] a = [a]
agregarAlFinal (x:xs) a = (x:xs) ++ [a]

--1.12
agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar (x:xs) ys = x : agregar xs ys

--1.13
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

--1.14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos (x:xs) [] = []
zipMaximos [] (y:ys) = []
zipMaximos [] [] = []
zipMaximos (x:xs) (y:ys) = max x y : zipMaximos xs ys

--1.15
elMinimo :: Ord a => [a] -> a
elMinimo (x:[]) = x
elMinimo (x:xs) = if x < (elMinimo xs) then x else elMinimo xs

--2.1
factorial :: Int -> Int
factorial 0 = 1
factorial n = factorial (n-1)*n

--2.2
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva  n = n : cuentaRegresiva (n-1)

--2.3
repetir :: Int -> a -> [a]
repetir 0 a = []
repetir n a = a : repetir (n-1) a

--2.4
losPrimeros :: Int -> [a] -> [a]
losPrimeros  n [] = []
losPrimeros 0 xs = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

--2.5
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros n [] = []
sinLosPrimeros 0 xs = xs
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs 

--3.1

data Persona = P String Int deriving Show

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA 0 xs = xs
mayoresA n (x:xs) = if (mayorA n x) then x : mayoresA n xs else mayoresA n xs



mayorA :: Int -> Persona -> Bool
mayorA 0 (P nom ed) = True
mayorA n (P nom ed) = n < ed

ale :: Persona
ale = P "Ale" 42

messi :: Persona
messi = P "Lio" 32

tito :: Persona
tito = P "Titus" 60

pety :: Persona
pety = P "Petty" 15


promedioEdad :: [Persona] -> Int
promedioEdad (x:[]) = edad x 
promedioEdad xs = promedio (edades xs)
 
promedio :: [Int] -> Int
promedio  [] = 0
promedio xs = div (sumatoria xs) (length xs)

edades :: [Persona] -> [Int]
edades  [] = []
edades  (x:xs) = (edad x) : edades xs 

elMasViejo :: [Persona] -> Persona
elMasViejo (x:[]) = x
elMasViejo (x:xs) = if (edad x > edad (elMasViejo xs))then x else elMasViejo xs

--3.2
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = CP TipoDePokemon Int
data Entrenador = CE String [Pokemon]

ron :: Entrenador
ron = CE "ron" [pikachu,wolf,can]

pikachu :: Pokemon
pikachu = CP Agua 2

can :: Pokemon
can = CP Fuego 5

wolf :: Pokemon
wolf = CP Fuego 4

cantPokemon :: Entrenador -> Int
cantPokemon (CE n []) = 0
cantPokemon (CE n xs) = longitud xs

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tp (CE n []) = 0
cantPokemonDe tp (CE n (x:xs)) = (sonDelMismoTipo tp x ) + cantPokemonDe tp (CE n (x:xs))

sonDelMismoTipo :: TipoDePokemon -> Pokemon -> Int
sonDelMismoTipo tp (CP tp1 e) = if (igualTipo tp tp1) then 1 else 0 

igualTipo :: TipoDePokemon -> TipoDePokemon -> Bool
igualTipo Agua Agua = True  
igualTipo Fuego Fuego = True
igualTipo Planta Planta = True
igualTipo _ _ = False


