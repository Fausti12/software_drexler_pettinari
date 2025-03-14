module Stack ( Stack, newS, freeCellsS, stackS, netS, holdsS, popS )
  where

import Palet
import Route

data Stack = Sta [ Palet ] Int deriving (Eq, Show)

newS :: Int -> Stack                      -- construye una Pila con la capacidad indicada  -> capacidad = cant Palets creo
newS capacity = Sta [] capacity 

freeCellsS :: Stack -> Int                -- responde con las celdas disponibles en la pila
freeCellsS (Sta listPalets capacity) =  capacity - length(listPalets)

stackS :: Stack -> Palet -> Stack         -- apila el palet indicado en la pila
-- debería llamar a holdsS? creo que no pq el usuario primero ve si se puede cargar y luego llama a stackS?
stackS (Sta listPalets capacity ) palet =  Sta (listPalets ++ [palet]) capacity


netS :: Stack -> Int                      -- responde el peso neto de los paletes en la pila
sumWeights :: [Palet] -> Int -> Int
sumWeights listPalets 0 = netP(listPalets !! 0)
sumWeights listPalets n = netP(listPalets !! n) + sumWeights listPalets (n-1)

netS (Sta listPalets _ ) = sumWeights listPalets (length(listPalets) - 1)


holdsS :: Stack -> Palet -> Route -> Bool -- indica si la pila puede aceptar el palet considerando las ciudades en la ruta
-- debo chequear free cells? , chequear si ciudad dest de Palet no está en lista ciudades en Route?
check lastPalet newPal rou = inOrderR(rou destinationP(lastPalet) destinationP(newPal)) -- True si newPal dest está después
holdsS (Sta listPalets capacity) pal rou | freeCellsS (Sta listPalets capacity ) == 0 = False
                                          | destinationP(last listPalets) == destinationP pal || freeCellsS(Sta listPalets capacity) == capacity = True  
                                          | otherwise = inOrderR rou (destinationP pal) (destinationP(last listPalets)) 
-- CON CHEQUEAR CON ULT PALET APILADO SERÍA SUFICIENTE


popS :: Stack -> String -> Stack          -- quita del tope los paletes con destino en la ciudad indicada
-- voy sacando desde atrás para adelante hasta que ciudad destino de algún palet cambie
-- como antes ya me aseguro que se apilen bien los palets, puedo simplemente buscar con "elem"= city hasta que de False?
countDestPalets:: [Palet] -> String -> Int
countDestPalets listPalets city | null listPalets = 0
                                | destinationP(last(listPalets)) == city = 1 + countDestPalets (init listPalets) city
                                | otherwise = 0


popS (Sta listPalets capacity) city = Sta (take(length(listPalets) - countDestPalets listPalets city)  listPalets) capacity
-- poner countDestPalets (listPalets city) -> toma listP como función (MAL)
-- IMPORTANTE: pongo en args a (Sta listPalets capacity) si uso sus partes solamente?