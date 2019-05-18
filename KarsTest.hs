import Kars
import Test.Hspec

-- hierro = Elemento "Hierro" "Fe" 26 Metal
-- helio  = Elemento "Helio"  "He"  2 GasNoble
-- fluor  = Elemento "Fluor"  "F"   9 Halogeno
-- cloro  = Elemento "Cloro"  "Cl" 17 Halogeno
-- sodio  = Elemento "Sodio"  "Na" 11 Metal

-- cloruroDeSodioComponentes = [(cloro, 1), (sodio, 1)]
-- cloruroDeSodio = Compuesto "Cloruro de Sodio" [(cloro, 1), (sodio, 1)] NoMetal

main :: IO ()
main = hspec $ do

  describe "Kars" $ do

    describe "TP 2" $ do

        describe "Punto 3.0" $ do

            it "La nafta de rochaMcQueen luego de hacer su truco favorito" $ do
            (nivelDeNafta . truco rochaMcQueen) rochaMcQueen `shouldBe` 300

            it "Nafta de rodra tras deReversa" $ do
            nivelDeNafta . deReversa rodra `shouldBe` 10
  
        describe "Punto 3.2" $ do

            it "Cantidad de participantes luego de sacarAlPistero en potreroFunes" $ do 
                ... `shouldBe` 3

            it "RochaMcQueen ya no participa en potreroFunes tras sacarAlPistero" $ do
                ... `shouldBe` True
  
            it "Cantidad de participantes luego de pocaReserva en potreroFunes" $ do
                ... `shouldBe` 3

            it "Rodra no está entre los participantes de potreroFunes luego de aplicar pocaReserva" $ do
                ... `shouldBe` False

            it "Cantidad de participantes luego de aplicar podio en potreroFunes" $ do
                ... `shouldBe` 3
            
            it "Velocidad del último participante de potreroFunes (rodra) luego de la lluvia" $ do
                ... `shouldBe` 40

        describe "Punto 3.3" $ do
            it "Nivel de nafta del primer participante (biankerr)  luego de dar una vuelta en potreroFunes" $ do
                ... `shouldBe` 490
            
            it "Velocidad del primer participante (biankerr) luego de dar una vuelta en potreroFunes" $ do
                ... `shouldBe` 40
            
            it "Cantidad de participantes tras dar dos vueltas en potreroFunes" $ do
                ... `shouldBe` 2
            
            it "Consultar el nivelDeNafta del primer participante (gushtav) luego de dos vueltas en potreroFunes" $ do
                ... `shouldBe` 70

            it "Rodra debe ser el único participante luego de correr la carrera de potreroFunes" $ do
                ... `shouldBe` rodra

        describe "Punto 3.4" $ do

            it "Ganador de potreroFunes" $ do
                ... `shouldBe` rodra
  
        describe "Punto 3.5" $ do

            it "Velocidad de rodra tras realizar elGranTruco con nitro, deReversa e impresionar" $ do
                ... `shouldBe` 130
    
            it "Nivel de nafta de rodra tras realizar elGranTruco con nitro, deReversa e impresionar" $ do
                ... `shouldBe` 13