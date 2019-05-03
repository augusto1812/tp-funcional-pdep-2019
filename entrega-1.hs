import Text.Show.Functions

-- PUNTO 1.1
-- Tipo de dato Auto
data Participante = Participante { 
                                  nombre :: String, 
                                  nivelDeNafta :: Int, 
                                  velocidad :: Int, 
                                  enamorade :: String}
                                  truco :: String}
                                  deriving (Show)
                                  
-- Autos
rochaMcQueen  = Participante { nombre = "Rocha McQueen", nivelDeNafta = 300, velocidad = 0, enamorade = "Ronco", truco = "deReversa" }

biankerr      = Participante { nombre = "Biankerr", nivelDeNafta = 500, velocidad = 20,  enamorade = "Tinch", truco = "impresionar" }

gushtav       = Participante { nombre = "Gushtav", nivelDeNafta = 200, velocidad = 130, enamorade = "PetiLaLinda", truco = "nitro"}

rodra         = Participante { nombre = "Rodra", nivelDeNafta = 0, velocidad = 50, enamorade = "Taisa", truco = "fingirAmor" }

-- PUNTO 1.2
-- Trucos
deReversa :: Participante -> Participante
deReversa uneParticipante = uneParticipante  {nivelDeNafta = ((+200).nivelDeNafta) uneParticipante}
-- Con la pista de 1000m, la función aumenta siempre 200 (1000/5) y sería
-- deReversa = (+200)

impresionar :: Participante -> Participante
impresionar uneParticipante = uneParticipante {velocidad = ((*2).velocidad) uneParticipante}

nitro :: Participante -> Participante
nitro uneParticipante = uneParticipante {velocidad = ((+15).velocidad) uneParticipante}

fingirAmor :: Participante -> String -> Participante 
fingirAmor uneParticipante nueveEnamorade = uneParticipante {enamorade = nueveEnamorade}

-- PUNTO 2
incrementarVelocidad :: Participante -> Participante
vocales :: String -> Bool
vocales  = any ( 'a' || 'e' || 'i' || 'o' || 'u' ) 

sacandoVocales :: Participante -> [Char]
sacandoVocales (Participante _ _ _ enamorade _ ) = filter (vocales) enamorade

largoDelNombreDeLaEnamorada :: [Char] -> Int
largoDelNombreDeLaEnamorada  = (length) . (sacandoVocales)

incrementarVelocidad :: Participante -> Participante
incrementarVelocidad (Participante _ _ velocidad enamorade _ ) = 
    (largoDelNombreDeLaEnamorada == 1)  (||) . (largoDelNombreDeLaEnamorada == 2) = nitro
incrementarVelocidad (Participante _ _ velocidad enamorade _ ) = 
    (largoDelNombreDeLaEnamorada == 3) (||) . (largoDelNombreDeLaEnamorada == 4) = velocidad + 20
incrementarVelocidad (Participante _ _ velocidad enamorade _ ) = 
    (largoDelNombreDeLaEnamorada > 4) = velocidad + 30- PUNTO 3

puedeRealizarTruco :: Participante -> Bool
puedeRealizarTruco (Participante _ nafta velocidad _ _) = (nafa != 0) && (velocidad < 100) 

cambiarEnamorada :: Participante -> Participante
cambiarEnamorada (Participante _ _ _ enamorade _ ) = fingirAmor

queTrucazo :: Participante -> Participante
queTrucazo (Participante _ _ velocidad enamorade _) = (incrementarVelocidad) . (cambiarEnamorada)

-- PUNTO 4
-- Nuevos trucos
comboLoco :: Participante -> Participante

queTrucazo :: Participante -> Participante

turbo :: Participante -> Participante
