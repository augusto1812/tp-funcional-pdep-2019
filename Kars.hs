import Text.Show.Functions

module Kars where

data Participante = 
    Participante { 
        nombre :: String, 
        nivelDeNafta :: Int, 
        velocidad :: Int, 
        enamorade :: Enamorade,
        truco :: (Participante -> Participante)
    } deriving (Show)

data Carreras = 
    Carreras {
        cantidadVueltas :: Int,
        longitudDePista :: Int,
        publico :: [Enamorade],
        trampa :: (Carreras-> Carreras),
        participantes :: [Participante]
    } deriving (Show)

data Enamorade = Roncho | Tinch | PetiLaLinda | Taisa deriving (Show, Eq)

---------------------------------------------------
---------------------- AUTOS ----------------------
---------------------------------------------------

rochaMcQueen  = Participante { 
    nombre = "Rocha McQueen",
    nivelDeNafta = 300,
    velocidad = 200,
    enamorade = "Ronco",
    truco = deReversa 
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
----------------------- 3.0 -----------------------
---------------------------------------------------

deReversa :: Participante -> Participante
deReversa (Participante nombre nivelDeNafta velocidad enamorade truco) = 
 	Participante nombre (nivelDeNafta + div velocidad 5) velocidad enamorade truco

---------------------------------------------------
----------------------- 3.2 -----------------------
---------------------------------------------------

neutralizarTrucos :: Carrera -> Carrera
neutralizarTrucos (Carrera cantidadVueltas longitudDePista publico trampa participantes) =
    Carrera cantidadVueltas longitudDePista publico trampa (map (modificarTruco inutilidad) participantes)

modificarTruco :: (Participante -> Participante) -> Participante -> Participante
modificarTruco nuevoTruco (Participante nombre nivelDeNafta velocidad enamorade truco) =
    Participante nombre nivelDeNafta velocidad enamorade nuevoTruco

pocaReserva :: Carrera -> Carrera 
pocaReserva (Carrera cantidadVueltas longitudDePista publico trampa participantes) =
    Carrera cantidadVueltas longitudDePista publico trampa (sacarParticipantesConNafta (<30) participantes)

sacarParticipantesConNafta :: Int -> [Participante] -> [Participante]
sacarParticipantesConNafta minNafta participantes =
    filter (minNafta.nivelDeNafta) participantes

podio :: Carrera -> Carrera
podio (Carrera cantidadVueltas longitudDePista publico trampa participantes) =
    Carrera cantidadVueltas longitudDePista publico trampa (take 3 participantes)

---------------------------------------------------
----------------------- 3.3 -----------------------
---------------------------------------------------

darVuelta :: Carrera -> Carrera
darVuelta (Carrera cantidadVueltas longitudDePista publico trampa participantes) =
    Carrera cantidadVueltas longitudDePista publico trampa ((sufrirPorParticipante trampa).(realizarTrucoSiHay publico).(restarCombustibleSegun longitudDePista) participantes)

-- restarCombustibleSegun :: Int -> [Participante] -> [Participante]
-- restarCombustibleSegun kms [(Participante nombre nivelDeNafta velocidad enamorade truco)] =
--     map (-kms/10*velocidad) [Participante nombre nivelDeNafta velocidad enamorade truco]

-- realizarTrucoSiHay :: [Enamorade] ->  [Participante] -> [Participante]
-- realizarTrucoSiHay publico [(Participante nombre nivelDeNafta velocidad enamorade truco)] =
--     map puedeRealizarTruco 

correrCarrera :: Carrera -> Carrera
correrCarrera (Carrera cantidadVueltas longitudDePista publico trampa participantes)
    | cantidadVueltas == 0 = (Carrera cantidadVueltas longitudDePista publico trampa participantes)
    | otherwise = darVuelta (Carrera cantidadVueltas-1 longitudDePista publico trampa participantes)