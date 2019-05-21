--module Kars where

import Text.Show.Functions

data Participante = 
    Participante { 
        nombre :: String, 
        nivelDeNafta :: Int, 
        velocidad :: Int, 
        enamorade :: String,
        truco :: (Participante -> Participante)
    } deriving (Show)

data Carrera = 
    Carrera {
        cantidadVueltas :: Int,
        longitudDePista :: Int,
        publico :: [String],
        trampa :: (Carrera -> Carrera),
        participantes :: [Participante]
    } deriving (Show)
    
---------------------------------------------------
---------------------- AUTOS ----------------------
---------------------------------------------------

rochaMcQueen  = Participante { 
    nombre = "Rocha McQueen",
    nivelDeNafta = 300,
    velocidad = 0,
    enamorade = "Ronco",
    truco = deReversa 
}

biankerr = Participante { 
    nombre = "Biankerr", 
    nivelDeNafta = 500, 
    velocidad = 20,  
    enamorade = "Tinch",
    truco = impresionar 
}

gushtav = Participante {
    nombre = "Gushtav", 
    nivelDeNafta = 200, 
    velocidad = 130, 
    enamorade = "PetiLaLinda",
    truco = nitro 
}

rodra = Participante { 
    nombre = "Rodra", 
    nivelDeNafta = 0, 
    velocidad = 50, 
    enamorade = "Taisa",
    truco = fingirAmor ("PetiLaLinda")
}

---------------------------------------------------
--------------------- CARRERA ---------------------
---------------------------------------------------

potreroFunes = Carrera {
    cantidadVueltas = 3,
    longitudDePista = 5,
    publico = ["Ronco", "Tinch", "Dodain"],
    trampa = sacarAlPistero,
    participantes = [rochaMcQueen, biankerr, gushtav, rodra]
}

---------------------------------------------------
----------------------- TP1 -----------------------
----------------------- 1.2 -----------------------
---------------------------------------------------

-- deReversa :: Participante -> Participante
-- deReversa (Participante nombre nivelDeNafta velocidad enamorade truco) = 
--     Participante nombre (nivelDeNafta+200) velocidad enamorade truco

impresionar :: Participante -> Participante
impresionar = modificarVelocidad (*2)

nitro :: Participante -> Participante
nitro = modificarVelocidad (+15)

fingirAmor :: String -> Participante -> Participante 
fingirAmor nueveEnamorade (Participante nombre nivelDeNafta velocidad enamorade truco) = 
    Participante nombre nivelDeNafta velocidad nueveEnamorade truco

---------------------------------------------------
----------------------- TP1 -----------------------
------------------------ 2 ------------------------
---------------------------------------------------

vocales = "aeiouAEIOU"

esVocal :: Char -> Bool
esVocal letra = 
    any (==letra) vocales

obtenerVocales :: Participante -> [Char]
obtenerVocales (Participante _ _ _ enamorade _) = 
    filter esVocal enamorade

cantidadDeVocalesDeEnamorade :: Participante -> Int
cantidadDeVocalesDeEnamorade  = length . obtenerVocales

incrementarVelocidad :: Participante -> Participante
incrementarVelocidad uneParticipante
    | (cantidadDeVocalesDeEnamorade uneParticipante == 1) || (cantidadDeVocalesDeEnamorade uneParticipante == 2) = modificarVelocidad (+15) uneParticipante
    | (cantidadDeVocalesDeEnamorade uneParticipante == 3) || (cantidadDeVocalesDeEnamorade uneParticipante == 4) = modificarVelocidad (+20) uneParticipante
    | (cantidadDeVocalesDeEnamorade uneParticipante > 4) = modificarVelocidad (+30) uneParticipante

---------------------------------------------------
----------------------- TP1 -----------------------
------------------------ 3 ------------------------
---------------------------------------------------

puedeRealizarTruco :: Participante -> Bool
puedeRealizarTruco uneParticipante = 
    ((&& (velocidad uneParticipante < 100)) . tieneNafta) uneParticipante

tieneNafta :: Participante -> Bool
tieneNafta (Participante _ nivelDeNafta _ _ _) =
    nivelDeNafta /= 0

---------------------------------------------------
----------------------- TP1 -----------------------
------------------------ 4 ------------------------
---------------------------------------------------

comboLoco :: Participante -> Participante
comboLoco = deReversa . nitro 

queTrucazo :: String -> Participante -> Participante
queTrucazo nueveEnamorade = 
  incrementarVelocidad . (fingirAmor nueveEnamorade)

turbo :: Participante -> Participante
turbo = terminarNafta . modificarVelocidad (*10)
    
---------------------------------------------------
----------------------- TP1 -----------------------
----------------------- AUX -----------------------
---------------------------------------------------

modificarVelocidad :: (Int -> Int) -> Participante -> Participante
modificarVelocidad func (Participante nombre nivelDeNafta velocidad enamorade truco) =
    Participante nombre nivelDeNafta (func velocidad) enamorade truco

terminarNafta :: Participante -> Participante
terminarNafta (Participante nombre nivelDeNafta velocidad enamorade truco) =
    Participante nombre 0 velocidad enamorade truco


---------------------------------------------------
----------------------- TP2 -----------------------
----------------------- 3.0 -----------------------
---------------------------------------------------

deReversa :: Participante -> Participante
deReversa (Participante nombre nivelDeNafta velocidad enamorade truco) = 
    Participante nombre (nivelDeNafta + div velocidad 5) velocidad enamorade truco

---------------------------------------------------
----------------------- TP2 -----------------------
----------------------- 3.2 -----------------------
---------------------------------------------------

sacarAlPistero :: Carrera -> Carrera
sacarAlPistero (Carrera cantidadVueltas longitudDePista publico trampa participantes) =
    Carrera cantidadVueltas longitudDePista publico trampa (tail participantes)
    

lluvia :: Carrera -> Carrera
lluvia (Carrera cantidadVueltas longitudDePista publico trampa participantes) =
    Carrera cantidadVueltas longitudDePista publico trampa (map (modificarVelocidad (+(-10))) participantes)


neutralizarTrucos :: Carrera -> Carrera
neutralizarTrucos (Carrera cantidadVueltas longitudDePista publico trampa participantes) =
    Carrera cantidadVueltas longitudDePista publico trampa (map (modificarTruco inutilidad) participantes)

modificarTruco :: (Participante -> Participante) -> Participante -> Participante
modificarTruco nuevoTruco (Participante nombre nivelDeNafta velocidad enamorade truco) =
    Participante nombre nivelDeNafta velocidad enamorade nuevoTruco

inutilidad :: Participante -> Participante
inutilidad uneParticipante = uneParticipante


pocaReserva :: Carrera -> Carrera 
pocaReserva (Carrera cantidadVueltas longitudDePista publico trampa participantes) =
    Carrera cantidadVueltas longitudDePista publico trampa (sacarParticipantesConNafta (<30) participantes)

sacarParticipantesConNafta :: (Int -> Bool) -> [Participante] -> [Participante]
sacarParticipantesConNafta minNafta participantes =
    filter (not.minNafta.nivelDeNafta) participantes


podio :: Carrera -> Carrera
podio (Carrera cantidadVueltas longitudDePista publico trampa participantes) =
    Carrera cantidadVueltas longitudDePista publico trampa (take 3 participantes)

---------------------------------------------------
----------------------- TP2 -----------------------
----------------------- 3.3 -----------------------
---------------------------------------------------

{--darVuelta :: Carrera -> Carrera
darVuelta (Carrera cantidadVueltas longitudDePista publico trampa participantes) =
  Carrera cantidadVueltas longitudDePista publico trampa ((sufrirPorParticipante trampa).(realizarTrucoSiHay publico).(restarCombustibleSegun longitudDePista) participantes)
--}

restarCombustibleDeLosParticipantes :: Int -> [Participante] -> [Participante]
restarCombustibleDeLosParticipantes kms lista =
 map (resComDeUnParticipante kms ) lista
 
resComDeUnParticipante :: Int -> Participante -> Participante
resComDeUnParticipante kms (Participante nombre nivelDeNafta velocidad enamorade truco) = Participante nombre (nivelDeNafta -((div kms 10 )* velocidad)) velocidad enamorade truco

{--realizarTrucoSiHay :: [Enamorade] ->  [Participante] -> [Participante]
realizarTrucoSiHay publico [(Participante nombre nivelDeNafta velocidad enamorade truco)] =
     map puedeRealizarTruco --}

{--puedeRealizarTruco :: [Enamorade] -> Participante -> Participante
puedeRealizarTruco publico (Participante nombre nivelDeNafta velocidad enamorade truco) = (Participante nombre nivelDeNafta velocidad (enamorade) truco)
 --}

--sufrirPorParticipante :: (Carrera -> Carrera) -> [Participante] -> [Participante] 
--sufrirPorParticipante trampaPista (Participante nombre nivelDeNafta velocidad enamorade truco)=(Participante nombre nivelDeNafta velocidad enamorade truco (trampaPista . trampa)) 
-- en este abria que ver si se puede agregar un campo al participante que sea trampa para poder hacerlo

{--correrCarrera :: Carrera -> Carrera
correrCarrera (Carrera cantidadVueltas longitudDePista publico trampa participantes)
     | cantidadVueltas == 0 = (Carrera cantidadVueltas longitudDePista publico trampa participantes)
     | otherwise = darVuelta (Carrera cantidadVueltas-1 longitudDePista publico trampa participantes)
--}
---------------------------------------------------
----------------------- TP2 -----------------------
----------------------- 3.4 -----------------------
---------------------------------------------------

{--quienGana :: Carrera -> Participante
quienGana = corredorConMayorVelocidad.correrCarrera
 
corredorConMayorVelocidad :: (Carrera ->[Participante]) -> Participante
corredorConMayorVelocidad corredores = filter ((=) . velocidadMaxima) corredores

velocidadMaxima :: (Carrera ->[Participante]) -> Int
velocidadMaxima corredores = maximum velocidad corredores   

corredores :: Carrera -> [Participante]
corredores (Carrera _ _ _ _ participantes) = participantes
--}
 

---------------------------------------------------
----------------------- TP2 -----------------------
----------------------- 3.5 -----------------------
---------------------------------------------------

elGranTruco :: [(Participante -> Participante)] -> Participante -> Participante
elGranTruco trucos uneParticipante =
    foldl1 (.) (reverse trucos) uneParticipante

---------------------------------------------------
----------------------- TP2 -----------------------
----------------------- 3.6 -----------------------
---------------------------------------------------

-- Para carrera con cantidad infinita de participantes:

-- A) ¿Podemos correrla?
-- Se puede correr pero no va a converger ya que siempre va a estar pidiendo un nuevo participante al cual realizarle la primera de las operaciones que solicita darVuelta. Por este motivo, en un momento dado se llenará la memoria y nos dará error.

-- B) ¿Podemos conocer el primer participante luego de 2 vueltas? 
-- No porque no se llega a dar dos vueltas. Sin embargo, se podría conocer el primer participante de la primera sin problemas.

-- C) ¿Podemos dar la primera vuelta de la carrera? 
-- No se puede llegar a concluir la primera vuelta de la carrera por lo explicado en el punto A.