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

data Carreras = 
    Carreras {
        cantidadVueltas :: Int,
        longitudDePista :: Int,
        publico :: [Enamorade],
        participantes :: [Participante],
        trampa :: (Carreras -> Carreras)
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


impresionar :: Participante -> Participante
impresionar = modificarVelocidad (*2)

nitro :: Participante -> Participante
nitro = modificarVelocidad (+15)

fingirAmor :: String -> Participante -> Participante 
fingirAmor nueveEnamorade uneParticipante = 
    uneParticipante {enamorade = nueveEnamorade}

modificarVelocidad :: (Int -> Int) -> Participante -> Participante
modificarVelocidad func (Participante nombre nivelDeNafta velocidad enamorade truco) =
    Participante nombre nivelDeNafta (func velocidad) enamorade truco


---------------------------------------------------
--------------------- CARRERA ---------------------
---------------------------------------------------

potreroFunes = Carreras {
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

------------ddads---------------------------------------
----------------------- 3.2 -----------------------
---------------------------------------------------
sacarAlPistero :: Carreras -> [Participante]
sacarAlPistero (Carreras _ _ _  participantes _ ) =
	  tail participantes

lluvia :: Participante -> Participante
lluvia = modificarVelocidad --nose|

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
