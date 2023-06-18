type Actor = String;
data Filmacion = Filmacion { titulo :: String, puntaje :: Int, lanzamiento :: Int, duracion :: Int, actores :: [Actor]} deriving Show

armaMortal = Filmacion "Arma Mortal" 7 1987 109 ["Mel Gibson", "Danny Glover", "Gary Busey"]
reinas = Filmacion "9 Reinas" 8 2000 114 ["Gastón Pauls", "Ricardo Darín", "Leticia Bredice","Pochi Ducasse"]
odisea = Filmacion "La odisea de los giles" 8 2019 116 ["Ricardo Darín", "Luis Brandoni", "Verónica Llinás", "Daniel Aráoz", "Rita Cortese"]
flor = Filmacion "La Flor" 7 2018 840 ["Pilar Gamboa"]
speed = Filmacion "Speed" 7 1994 116 ["Keanu Reeves", "Sandra Bullock", "Dennis Hopper", "Jeff Daniels", "Alan Ruck"]
indianaIV = Filmacion "Indiana Jones IV" 6 2007 125 ["Harrison Ford"]
indianaI =  Filmacion "Indiana Jones I" 8 1981 115 ["Harrison Ford"]

{- Ejer1 -}
esDarinesca :: Filmacion -> Bool
esDarinesca filme = (=="Ricardo Darín").head.actores $ filme

pintaBuena :: Filmacion -> Bool
pintaBuena filme = (>=5).length.actores $ filme

minutosExcedentes :: Filmacion -> Int
minutosExcedentes filme = abs (duracion filme - 115)

{- Ejer2 -}
pintaGrosa :: Filmacion -> Bool
pintaGrosa filme = (>4).length.actores $ filme

esVieja :: Filmacion -> Bool
esVieja filme = (<1990).lanzamiento $ filme

titleLenPorDos :: Filmacion -> Int
titleLenPorDos filme = (*2).length.titulo $ filme

puntajePorTres :: Filmacion -> Int
puntajePorTres filme = (*3).puntaje $ filme

addSignoPesos :: Int -> String
addSignoPesos num = "$" ++ show num

precioBase :: Filmacion -> Int
precioBase filme | pintaGrosa filme = 200 | esVieja filme = titleLenPorDos filme | otherwise = 100 + puntajePorTres filme

esLarga :: Filmacion -> Bool
esLarga filme = (>115).duracion $ filme

extraPorExcedente :: Filmacion -> Int
extraPorExcedente filme = ((*10).minutosExcedentes $ filme) `min` 100

precioExtra :: Filmacion -> Int
precioExtra filme | esLarga filme = extraPorExcedente filme | not.esVieja $ filme = 50 | otherwise = 0

calcTotal :: Filmacion -> Int
calcTotal filme = precioBase filme + precioExtra filme

aplicarDto :: Int -> Int
aplicarDto precio = round (fromIntegral precio * 0.9)

precioTotal :: Filmacion -> Int
precioTotal filme | calcTotal filme > 200 = aplicarDto.calcTotal $ filme | otherwise = calcTotal filme

{- Ejer3 -}

