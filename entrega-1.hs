import Text.Show.Functions

-- PUNTO 1.1
data Participante = Participante { 
                                  nombre :: String, 
                                  nivelDeNafta :: Int, 
                                  velocidad :: Int, 
                                  enamorade :: String,
                                  truco :: (Participante -> Participante)
                                  } deriving (Show)
                                  
-- Autos
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
    truco = (fingirAmor "Petra")
}

-- PUNTO 1.2
deReversa :: Participante -> Participante
deReversa (Participante nombre nivelDeNafta velocidad enamorade truco) = 
    Participante nombre (nivelDeNafta+200) velocidad enamorade truco

impresionar :: Participante -> Participante
impresionar = modificarVelocidad (*2)

nitro :: Participante -> Participante
nitro = modificarVelocidad (+15)

fingirAmor :: String -> Participante -> Participante 
fingirAmor nueveEnamorade uneParticipante = 
    uneParticipante {enamorade = nueveEnamorade}

hazLoTuyo :: Participante -> Participante
hazLoTuyo uneParticipante =
    (truco uneParticipante) $ uneParticipante  

-- PUNTO 2
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
  
-- PUNTO 3
puedeRealizarTruco :: Participante -> Bool
puedeRealizarTruco uneParticipante = 
    ((&& (velocidad uneParticipante < 100)) . tieneNafta) uneParticipante

tieneNafta :: Participante -> Bool
tieneNafta (Participante _ nivelDeNafta _ _ _) =
    nivelDeNafta /= 0

-- PUNTO 4
comboLoco :: Participante -> Participante
comboLoco = deReversa . nitro 

queTrucazo :: String -> Participante -> Participante
queTrucazo nueveEnamorade = 
  incrementarVelocidad . (fingirAmor nueveEnamorade)

turbo :: Participante -> Participante
turbo = terminarNafta . modificarVelocidad (*10)
    
-- AUXILIARES

modificarVelocidad :: (Int -> Int) -> Participante -> Participante
modificarVelocidad func (Participante nombre nivelDeNafta velocidad enamorade truco) =
    Participante nombre nivelDeNafta (func velocidad) enamorade truco

terminarNafta :: Participante -> Participante
terminarNafta (Participante nombre nivelDeNafta velocidad enamorade truco) =
    Participante nombre 0 velocidad enamorade truco