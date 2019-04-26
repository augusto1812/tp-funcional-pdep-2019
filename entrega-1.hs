import Text.Show.Functions

--PUNTO 1: Modelado de autos y trucos

data Participante = Participante { 
                                  nombre :: String, 
                                  nivelDeNafta :: Int, 
                                  velocidad :: Int, 
                                  enamorade :: Participante}
                                  -- truco :: }
                                  deriving (Show)
                                  
--Autos

rochaMcQueen = Participante { nombre = "Rocha McQueen", nivelDeNafta = 300, velocidad = 0 enamorade = "ronco" -- truco = deReversa }

--No entiendo si su enamorade va a ser otro participante (de tipo data) o solo ponemos el nombre --de su enamorade ( o sea, un String)

biankerr = Participante { nombre = "Biankerr", nivelDeNafta = 500, velocidad = 20  enamorade = "tinch" --truco = impresionar }

gushtav = Participante { nombre = "Gushtav", nivelDeNafta = 200, velocidad = 130 enamorade = "PetiLaLinda" -- truco = nitr }

rodra = Participante { nombre = "Rodra", nivelDeNafta = 0, velocidad = 50 enamorade = "Taisa" -- truco = fingirAmor con petra ??? no se como hacer esto ayuda }

--PUNTO 1: 2) 
deReversa :: Participante -> Participante
deReversa unParticipante = unParticipante  {nivelDeNafta = ((+200).nivelDeNafta) unParticipante}
 --preguntar por qué te dice que la pista tiene 100m, o sea que siempre la distancia va a ser 200 (1/5 de mil) 
 --sino seria esta la funcion: -- deReversa = (+200)
nitro :: Participante-> Participante
impresionar unParticipante = unParticipante {velocidad = ((*2).velocidad) unParticipante}

nitro :: Participante-> Participante
nitro unParticipante = unParticipante {velocidad = ((+15).velocidad) unParticipante}

--no se como hacer fingir amor


 
