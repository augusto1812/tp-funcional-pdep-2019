import Text.Show.Functions

--PUNTO 1: Modelado de autos y trucos

data Participante = Participante { 
                                  nombre :: String, 
                                  nivelDeNafta :: Int, 
                                  velocidad :: Int, 
                                  enamorade :: Participante
                                  -- truco :: } 
                                  deriving (Show)

--PUNTO 1: 2) 
 deReversa :: Float -> Float -> Float 
 deReversa nivelDeNafta = (+nivelDeNafta).(/5) 
 --preguntar por qué te dice que la pista tiene 100m, o sea que siempre la distancia va a ser 200 (1/5 de mil) 
 --sino seria esta la funcion: -- deReversa = (+200)

 impresionar :: Int -> Int 
 impresionar = (*2)

 nitro :: Int -> Int 
 nitro = (+15)

--no se como hacer fingir amor

--PUNTO 1: 3) 
 rochaMcQueen = Participante { nombre = "Rocha McQueen", nivelDeNafta = 300, velocidad = 0 -- enamorade = "ronco" -- truco = deReversa }

--No entiendo si su enamorade va a ser otro participante (de tipo data) o solo ponemos el nombre --de su enamorade ( o sea, un String)

 biankerr = Participante { nombre = "Biankerr", nivelDeNafta = 500, velocidad = 20 -- enamorade = tinch --truco = impresionar }

 gushtav = Participante { nombre = "Gushtav", nivelDeNafta = 200, velocidad = 130 -- enamorada = PetiLaLinda -- truco = nitr }

 rodra = Participante { nombre = "Rodra", nivelDeNafta = 0, velocidad = 50 -- enamorada = Taisa -- truco = fingirAmor con petra ??? no se como hacer esto ayuda }
