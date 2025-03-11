module Palet ( Palet, newP, destinationP, netP ) where -- para poder exportar funciones

data Palet = Pal String Int deriving (Eq, Show)  -- "deriving..." permite compararse con == y /= (Eq)
-- y convertir en una cadena (Show), lo que permite hacer print palet1

newP :: String -> Int -> Palet -- construye un Palet dada una ciudad de destino y un peso en toneladas
newP dest weight = Pal dest weight


destinationP :: Palet -> String  -- responde la ciudad destino del palet
destinationP (Pal dest _) = dest

netP :: Palet -> Int  -- responde el peso en toneladas del palet
netP (Pal _ weight) = weight

-- usar "let p = newP ..." en terminal