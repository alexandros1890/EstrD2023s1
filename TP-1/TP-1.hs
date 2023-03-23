--1
--a)
sucesor :: Int -> Int
sucesor n = n + 1
--b)
sumar :: Int -> Int -> Int
sumar n m = n + m
--c)
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = ((div n m), (mod n m))
--d) 
maxDelPar :: (Int,Int) -> Int
maxDelPar (n,m) = if (n > m) 
    then n 
    else m  

--2
--a)
sucesor (sumar 5 maxDelPar (4, (divisionYResto 2 2)))
--b)
maxDePar (divisionYResto ((sumar 5 5), (sucesor 0)))
--c)
divisionYResto (sumar 9 (maxDelPar (0 (sucesor 0))))
--d)
sumar 5 (maxDePar (divisionYResto 10  (sucesor 3)))

--3.1

--a)
data Dir = Norte | Este | Sur | Oeste deriving Show

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este
--b)
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este= True
iguales Oeste Oeste = True
iguales _ _ = False
--c)
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
--Precond: no hay siguiente de Oeste

--Esta es una función parcial porque tiene una precondición.


--3.2
--a)
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)
--b)
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False
--c)
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes Martes = True
vieneDespues Martes Miercoles = True
vieneDespues Miercoles Jueves = True
vieneDespues Jueves Viernes = True
vieneDespues Viernes Sabado = True
vieneDespues Sabado Domingo = True
vieneDespues Domingo Lunes = True
vieneDespues _ _ = False
--d)
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Martes  = True
estaEnElMedio Miercoles  = True
estaEnElMedio Jueves  = True
estaEnElMedio Viernes = True
estaEnElMedio Sabado = True
estaEnElMedio _  = False

--3.3
--a)
negar :: Bool -> Bool
negar n =  not n
--b)
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _= True
--c)
yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _= False
--d)
oBien :: Bool -> Bool -> Bool
oBien False False = False
oBien _ _= True

--4.1

data Persona = P String Int deriving Show

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona -> Persona
crecer ( P n e) = (P n (e+1))

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nom (P n e) = ( P nom e)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P n e)  ( P n1 e1) = e > e1

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor (P n e)  ( P n1 e1) = (
    if( e > e1) 
        then (P n e)
        else ( P n1 e1) )

luk :: Persona  
luk = P "luk" 24
sky :: Persona  
sky = P "sky" 42



--4.2

data TipoDePokemon = Agua | Fuego | Planta  deriving (Eq, Show)
data Pokemon = PK TipoDePokemon Int deriving Show
data Entrenador = E String Pokemon Pokemon deriving Show

entrenador1 :: Entrenador
entrenador1 = E "Nico" pok1 pok2
entrenador2 :: Entrenador
entrenador2 = E "Gus" pok2 pok3
entrenador3 :: Entrenador
entrenador3 = E "Isra" pok1 pok4

pok1 :: Pokemon 
pok1 = PK Agua 100 
pok2 :: Pokemon 
pok2 = PK Fuego 80
pok3 :: Pokemon 
pok3 = PK Planta 90 
pok4 :: Pokemon 
pok4 = PK Agua 70


superaA :: Pokemon -> Pokemon -> Bool
superaA (PK tp ent) (PK tp1 ent1) = tipoDePokemonEsSuperior tp tp1

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tp (E _ pk1 pk2) = (sonDelMismoTipo tp pk1) + (sonDelMismoTipo tp pk2)

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ((E _ pk1 pk2), (E _ pk3 pk4)) = pk1 : pk2 : pk3 : pk4 : []
juntarPokemon _ = []

--5

loMismo :: a -> a
loMismo a = a

siempreSiete :: a -> Int
siempreSiete a = 7

swap :: (a,b) -> (b, a)
swap  (a, b) = (b, a)

--Las funciones son polimorficas porque todas pueden recibir los mismos parametros.

--6
 
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False



elPrimero :: [a] -> a
elPrimero (x:xs) = x
-- Precond: la lista no tiene que estar vacia


sinElPrimero :: [a] -> [a]
sinElPrimero  (x:xs) = xs
-- Precond: la lista no tiene que estar vacia

splitHead :: [a] -> (a, [a])
splitHead  (x:xs) = (x, sinElPrimero (x: xs))




