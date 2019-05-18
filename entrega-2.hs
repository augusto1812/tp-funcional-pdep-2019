import Text.Show.Functions
data Participante = Participante { 
                                  nombre :: String, 
                                  nivelDeNafta :: Int, 
                                  velocidad :: Int, 
                                  enamorade :: String,
                                  truco :: (Participante -> Participante)
                                  } deriving (Show)
data Carreras = Carreras {
						cantidadVueltas:: Int,
						longitudDePista:: Int,
						publico:: [String],
						trampa::(Carreras-> Carreras),
						participantes:: [Participante]
					    } deriving (Show)



-- Autos
rochaMcQueen  = Participante { 
    nombre = "Rocha McQueen",
    nivelDeNafta = 300,
    velocidad = 200,
    enamorade = "Ronco",
    truco = deReversa 
}



-- PUNTO 0
deReversa :: Participante -> Participante
deReversa (Participante nombre nivelDeNafta velocidad enamorade truco ) = 
 	Participante nombre (nivelDeNafta + div velocidad 5) velocidad enamorade truco
 	