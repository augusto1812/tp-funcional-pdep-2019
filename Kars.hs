import Text.Show.Functions

--module Kars where

data Participante = 
    Participante { 
        nombre :: String, 
        nivelDeNafta :: Int, 
        velocidad :: Int, 
        enamorade :: String,
        truco :: (Participante -> Participante)
    } deriving (Show)

data Carreras = 
    Carreras {
        cantidadVueltas :: Int,
        longitudDePista :: Int,
        publico :: [String],
        participantes :: [Participante],
        trampa :: (Carreras-> [Participante])
    } deriving (Show)

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
deReversa (Participante nombre nivelDeNafta velocidad enamorade truco ) = 
 	Participante nombre (nivelDeNafta + div velocidad 5) velocidad enamorade truco

------------ddads---------------------------------------
----------------------- 3.2 -----------------------
---------------------------------------------------
sacarAlPistero :: Carreras -> [Participante]
sacarAlPistero (Carreras _ _ _  participantes _ ) =
	  tail participantes
podio :: Carreras -> [Participante]
podio (Carreras _ _ _  participantes _) = 
     take 3 participantes
lluvia :: Participante -> Participante
lluvia = modificarVelocidad --nose|