import Text.Show.Functions

--module Kars where

data Participante = 
    Participante { 
        nombre :: String, 
        nivelDeNafta :: Int, 
        velocidad :: Int, 
        enamorade :: Enamorade,
        truco :: (Participante -> Participante)
    } deriving (Show)

data Carrera = 
    Carrera {
        cantidadVueltas :: Int,
        longitudDePista :: Int,
        publico :: [Enamorade],
        trampa :: (Carrera -> Carrera),
        participantes :: [Participante]
    } deriving (Show)

data Enamorade = Ronco | Tinch | PetiLaLinda | Taisa | Dodain deriving (Show, Eq)
    
---------------------------------------------------
---------------------- AUTOS ----------------------
---------------------------------------------------

rochaMcQueen  = Participante { 
    nombre = "Rocha McQueen",
    nivelDeNafta = 300,
    velocidad = 200,
    enamorade = Ronco,
    truco = deReversa 
}

biankerr = Participante { 
    nombre = "Biankerr", 
    nivelDeNafta = 500, 
    velocidad = 20,  
    enamorade = Tinch,
    truco = impresionar 
}

gushtav = Participante {
    nombre = "Gushtav", 
    nivelDeNafta = 200, 
    velocidad = 130, 
    enamorade = PetiLaLinda,
    truco = nitro 
}

rodra = Participante { 
    nombre = "Rodra", 
    nivelDeNafta = 0, 
    velocidad = 50, 
    enamorade = Taisa,
    truco = fingirAmor (PetiLaLinda)
}

---------------------------------------------------
--------------------- CARRERA ---------------------
---------------------------------------------------

potreroFunes = Carrera {
    cantidadVueltas = 3,
    longitudDePista = 5,
    publico = [Ronco, Tinch, Dodain],
    trampa = sacarAlPistero,
    participantes = [rochaMcQueen, biankerr, gushtav, rodra]
}

---------------------------------------------------
--------------------- DEL TP1 ---------------------
---------------------------------------------------

impresionar :: Participante -> Participante
impresionar = modificarVelocidad (*2)

nitro :: Participante -> Participante
nitro = modificarVelocidad (+15)

fingirAmor :: Enamorade -> Participante -> Participante 
fingirAmor nueveEnamorade (Participante nombre nivelDeNafta velocidad enamorade truco) = 
    Participante nombre nivelDeNafta velocidad nueveEnamorade truco

modificarVelocidad :: (Int -> Int) -> Participante -> Participante
modificarVelocidad func (Participante nombre nivelDeNafta velocidad enamorade truco) =
    Participante nombre nivelDeNafta (func velocidad) enamorade truco

---------------------------------------------------
----------------------- 3.0 -----------------------
---------------------------------------------------

deReversa :: Participante -> Participante
deReversa (Participante nombre nivelDeNafta velocidad enamorade truco) = 
    Participante nombre (nivelDeNafta + div velocidad 5) velocidad enamorade truco

---------------------------------------------------
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
----------------------- 3.5 -----------------------
---------------------------------------------------
{--elGranTruco :: [Participante -> Participante] -> Participante -> Participante
elGranTruco trucos (Participante nombre nivelDeNafta velocidad enamorade truco) = Participante--}
