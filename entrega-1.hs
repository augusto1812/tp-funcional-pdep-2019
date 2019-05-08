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
deReversa uneParticipante = 
    uneParticipante  {nivelDeNafta = ((+200).nivelDeNafta) uneParticipante}

impresionar :: Participante -> Participante
impresionar uneParticipante = 
    uneParticipante {velocidad = ((*2).velocidad) uneParticipante}

nitro :: Participante -> Participante
nitro uneParticipante = 
    uneParticipante {velocidad = ((+15).velocidad) uneParticipante}

fingirAmor :: String -> Participante -> Participante 
fingirAmor nueveEnamorade uneParticipante = 
    uneParticipante {enamorade = nueveEnamorade}

hazLoTuyo :: Participante -> Participante
hazLoTuyo uneParticipante =
    (truco uneParticipante) $ uneParticipante  

-- PUNTO 2
esVocal :: Char -> Bool
esVocal letra = 
    ((letra == 'a') || (letra =='e') || (letra =='i') || (letra =='o') || (letra =='u'))

obtenerVocales :: Participante -> [Char]
obtenerVocales (Participante _ _ _ enamorade _) = 
    filter (esVocal) enamorade

cantidadDeVocalesDeEnamorade :: Participante -> Int
cantidadDeVocalesDeEnamorade  = length . obtenerVocales

incrementarVelocidad :: Participante -> Participante
incrementarVelocidad uneParticipante
    | (cantidadDeVocalesDeEnamorade uneParticipante == 1) || (cantidadDeVocalesDeEnamorade uneParticipante == 2) = uneParticipante {velocidad = ((+15).velocidad) uneParticipante}
    | (cantidadDeVocalesDeEnamorade uneParticipante == 3) || (cantidadDeVocalesDeEnamorade uneParticipante == 4) = uneParticipante {velocidad = ((+20).velocidad) uneParticipante}
    | (cantidadDeVocalesDeEnamorade uneParticipante > 4) = uneParticipante {velocidad = ((+30).velocidad) uneParticipante}
  
-- PUNTO 3
puedeRealizarTruco :: Participante -> Bool
puedeRealizarTruco (Participante _ nivelDeNafta velocidad _ _) = 
    (nivelDeNafta /= 0) && (velocidad < 100)

-- PUNTO 4
comboLoco :: Participante -> Participante
comboLoco = deReversa.nitro 

queTrucazo :: String -> Participante -> Participante
queTrucazo nueveEnamorade = 
  incrementarVelocidad.(fingirAmor nueveEnamorade)

turbo :: Participante -> Participante
turbo uneParticipante =
    uneParticipante {velocidad = velocidad uneParticipante + ((*10).nivelDeNafta) uneParticipante, nivelDeNafta = 0}
    
