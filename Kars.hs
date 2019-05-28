module Kars where

import Text.Show.Functions
import Data.List

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
        trampa :: ([Participante] -> [Participante]),
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

sacarAlPistero :: [Participante] -> [Participante]
sacarAlPistero participantes = tail participantes
    

lluvia :: [Participante] -> [Participante]
lluvia participantes= (map (modificarVelocidad (+(-10))) participantes)


neutralizarTrucos :: [Participante] -> [Participante]
neutralizarTrucos participantes = (map (modificarTruco inutilidad) participantes)

modificarTruco :: (Participante -> Participante) -> Participante -> Participante
modificarTruco nuevoTruco (Participante nombre nivelDeNafta velocidad enamorade truco) =
    Participante nombre nivelDeNafta velocidad enamorade nuevoTruco

inutilidad :: Participante -> Participante
inutilidad uneParticipante = uneParticipante


pocaReserva :: [Participante] -> [Participante] 
pocaReserva participantes = (sacarParticipantesConNafta (<30) participantes)

sacarParticipantesConNafta :: (Int -> Bool) -> [Participante] -> [Participante]
sacarParticipantesConNafta minNafta participantes =
    filter (not.minNafta.nivelDeNafta) participantes


podio :: [Participante] -> [Participante]
podio participantes = take 3 participantes
---------------------------------------------------
----------------------- TP2 -----------------------
----------------------- 3.3 -----------------------
---------------------------------------------------

--PUNTO 1--
restarCombustibleDeLosParticipantes :: Carrera -> Carrera
restarCombustibleDeLosParticipantes (Carrera cantidadVueltas longitudDePista publico trampa participantes) = Carrera cantidadVueltas longitudDePista publico trampa  (map (resComDeUnParticipante longitudDePista ) participantes)
resComDeUnParticipante :: Int -> Participante -> Participante
resComDeUnParticipante kms (Participante nombre nivelDeNafta velocidad enamorade truco) = Participante nombre (nivelDeNafta -((div kms 10 )* velocidad)) velocidad enamorade truco


--PUNTO 2--


estaElEnamoradeYPuedeRealizarTruco :: Participante -> [String]  -> [Participante]
estaElEnamoradeYPuedeRealizarTruco (Participante nombre nivelDeNafta velocidad enamorade truco) publico
 |(elem enamorade publico) && (puedeRealizarTruco (Participante nombre nivelDeNafta velocidad enamorade truco))  = [(hazLoTuyo (Participante nombre nivelDeNafta velocidad enamorade truco))]
 |otherwise = [(Participante nombre nivelDeNafta velocidad enamorade truco)]

mapeaParticipantes :: [Participante] -> [String]-> [Participante]
mapeaParticipantes [] publico = []
mapeaParticipantes (x:xs) publico = (estaElEnamoradeYPuedeRealizarTruco x publico) ++ (mapeaParticipantes xs publico)

hazLoTuyo :: Participante -> Participante
hazLoTuyo uneParticipante =
    (truco uneParticipante) $ uneParticipante  

realizarTrucoSiHay :: Carrera -> Carrera 
realizarTrucoSiHay (Carrera cantidadVueltas longitudDePista publico trampa participantes)  = Carrera cantidadVueltas longitudDePista publico trampa (mapeaParticipantes participantes publico)

--PUNTO 3--
sufrirTrampaDeCarrera :: Carrera -> Carrera
sufrirTrampaDeCarrera (Carrera cantidadVueltas longitudDePista publico trampa participantes) = Carrera cantidadVueltas longitudDePista publico trampa (trampa participantes) 


--PUNTO 4--
darVuelta :: Carrera -> Carrera
darVuelta carrera = sufrirTrampaDeCarrera.realizarTrucoSiHay.restarCombustibleDeLosParticipantes $  carrera
correrCarrera:: Carrera -> [Carrera]
correrCarrera (Carrera cantidadVueltas longitudDePista publico trampa participantes) = take cantidadVueltas $ iterate (darVuelta) (darVuelta (Carrera cantidadVueltas longitudDePista publico trampa participantes) )

---------------------------------------------------
----------------------- TP2 -----------------------
----------------------- 3.4 -----------------------
---------------------------------------------------

obtenerUltimaVuelta :: Carrera -> Carrera
obtenerUltimaVuelta carrera = last (correrCarrera carrera) 

--participanteConMayorVelocidad :: [Participante] -> Participante
--participanteConMayorVelocidad participantes = filter (== participante velocidad) participantes

maximaVelocidad ::Carrera -> Int
maximaVelocidad (Carrera _ _ _ _ participantes) = maximum (map (velocidad) participantes) 


--quienGana :: Carrera -> Participante
--quienGana carrera = --}
--ordenarVelocidad participante1 participante2 
 --   | velocidad participante1 > velocidad participante2 = GT
  --  | velocidad participante1 <= velocidad participante2 = LT


quienGana :: Carrera -> Int
quienGana carrera = maximaVelocidad.obtenerUltimaVuelta $ carrera

---------------------------------------------------
----------------------- TP2 -----------------------
----------------------- 3.5 -----------------------
---------------------------------------------------

elGranTruco :: [(Participante -> Participante)] -> Participante -> Participante
elGranTruco trucos uneParticipante =
    foldl1 (.) trucos uneParticipante

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