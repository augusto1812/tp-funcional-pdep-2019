# Correcciones

## Consigna:
### Punto 1: Modelado de autos y trucos
1. Modelar el tipo de dato auto
	 ```haskell
	 data Participante = Participante {
	nombre :: String,
	nivelDeNafta :: Int,
	velocidad :: Int,
	enamorade :: String,
	truco :: (Participante -> Participante)
	} deriving (Show)
	```
	*Correcciones*: **ninguna**

    *Observaciones*: **ninguna**

2. Modelar los trucos deReversa, impresionar, nitro y fingirAmor.
    ```haskell
    deReversa :: Participante -> Participante
    deReversa uneParticipante =
      uneParticipante {nivelDeNafta = ((+200).nivelDeNafta) uneParticipante}

    impresionar :: Participante -> Participante
    impresionar uneParticipante =
      uneParticipante {velocidad = ((*2).velocidad) uneParticipante}

    nitro :: Participante -> Participante
    nitro uneParticipante =
      uneParticipante {velocidad = ((+15).velocidad) uneParticipante}

    fingirAmor :: String -> Participante -> Participante
    fingirAmor nueveEnamorade uneParticipante =
      uneParticipante {enamorade = nueveEnamorade}
      ```
    *Correcciones*: **ninguna**.

    *Observaciones*: **ninguna**

4. Modelar los siguientes autos:
	- RochaMcQueen que tiene 300 litros, su velocidad inicial es 0, su enamorado es Ronco y su truco es deReversa.
	- Biankerr (nuestro tanque ruso) tiene 500 litros, su velocidad inicial es 20, su enamorado es Tinch y su truco es impresionar.
	- Gushtav tiene 200 litros, su velocidad inicial es 130, su enamorada es PetiLaLinda y su truco es nitro.
	- Rodra que se olvidó de cargar nafta (tiene 0 litros), su velocidad inicial es 50, su enamorada es Taisa y su truco es fingirAmor con Petra.
    ```haskell
      rochaMcQueen = Participante {
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
     ```
     *Correcciones*: **ninguna**.

     *Observaciones*: **ninguna**

### Punto 2: Incrementar velocidad

La velocidad incrementa también según la cantidad de vocales de le enamorade de su auto:
-   Si tiene entre 1 y 2 letras aumenta 15 km/h.
-   Si tiene entre 3 y 4 aumenta 20 km/h.
  -   Si tiene más de 4 aumenta 30 km/h.
  ```haskell
    esVocal :: Char -> Bool
    esVocal letra =
    ((letra == 'a') || (letra =='e') || (letra =='i') || (letra =='o') || (letra =='u'))

    obtenerVocales :: Participante -> [Char]
    obtenerVocales (Participante _ _ _ enamorade _) = filter (esVocal) enamorade

    cantidadDeVocalesDeEnamorade :: Participante -> Int
    cantidadDeVocalesDeEnamorade = length . obtenerVocales
    incrementarVelocidad :: Participante -> Participante
    incrementarVelocidad uneParticipante
    | (cantidadDeVocalesDeEnamorade uneParticipante == 1) || (cantidadDeVocalesDeEnamorade uneParticipante == 2) = uneParticipante {velocidad = ((+15).velocidad) uneParticipante}
    | (cantidadDeVocalesDeEnamorade uneParticipante == 3) || (cantidadDeVocalesDeEnamorade uneParticipante == 4) = uneParticipante {velocidad = ((+20).velocidad) uneParticipante}
    | (cantidadDeVocalesDeEnamorade uneParticipante > 4) = uneParticipante {velocidad = ((+30).velocidad) uneParticipante}
  ```
  *Observaciones*: se pueden sacar los paréntesis de ```filter (esVocal) enamorade```.

  *Correcciones*:  Es **esVocal** puede quedar mucho más simple si utilizan un string definido como ```vocales = "aeiou"```; piensen qué hay que hacer a partir de esa *lista*. Los casos para las cantidades de vocales pueden resolverse sin usar ```||```: ¿se les ocurre cómo? Les va a quedar también mucho más corto. Además, se repite la lógica para aumentar la velocidad, que es algo que ocurre muchas veces en el TP. Abstráiganlo en una nueva función.

### Punto 3: Puede realizar truco
Agregar la posibilidad de preguntar si un auto puede realizar un truco. Esto es posible si tiene nafta en el tanque y si su velocidad es menor a 100.
  ```haskell
    puedeRealizarTruco (Participante _ nivelDeNafta velocidad _ _) = (nivelDeNafta /= 0) && (velocidad < 100)
  ```
  *Observaciones*: **ninguna**

  *Correcciones*:  Resolver con composición.

### Punto 4: Nuevos trucos    
-   comboLoco que es realizar deReversa con nitro.
-   queTrucazo primero cambia de enamorade y luego utiliza incrementar velocidad.
-   turbo lleva la nafta a 0 y aumenta la velocidad en la cantidad de nafta que tenía (*10).
  ```haskell
    comboLoco :: Participante -> Participante
    comboLoco = deReversa.nitro

    queTrucazo :: String -> Participante -> Participante
    queTrucazo nueveEnamorade =
      incrementarVelocidad.(fingirAmor nueveEnamorade)

    turbo :: Participante -> Participante
    turbo uneParticipante =
      uneParticipante {velocidad = ((*10).nivelDeNafta) uneParticipante, nivelDeNafta = 0}
  ```
*Observaciones*: **ninguna**

*Correccion*: Los tests que chequean la velocidad con ```turbo``` están dando mal. Revean qué es lo que les pide, porque están omitiendo una suma.
