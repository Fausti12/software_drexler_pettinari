module Main where

import Palet
import Route
import Stack
import Truck

main :: IO ()
main = do
    let palet1 = newP "CiudadA" 3
        palet2 = newP "CiudadB" 5
        palet3 = newP "CiudadC" 2

        route = newR ["CiudadA", "CiudadB", "CiudadC", "CiudadD"]

        stack = newS 5
        stack1 = stackS stack palet1
        stack2 = stackS stack1 palet2
        stack3 = stackS stack2 palet3

        truck = newT 2 5 route
        l = freeCellsT truck
        --truck1 = loadT truck palet1
        --truck2 = loadT truck1 palet2
        -- truck3 = loadT truck2 palet3

    print truck
    print l


-- Crear una ruta
let route = newR ["Buenos Aires", "Cordoba", "Rosario"]

-- Crear algunos palets
let palet1 = newP "Buenos Aires" 3
let palet2 = newP "Cordoba" 4
let palet3 = newP "Rosario" 2
let palet4 = newP "Cordoba" 5

-- Crear un camión con 3 bahías, cada una con una altura de 10, y la ruta creada anteriormente
let truck = newT 3 10 route

-- Verificar las celdas libres en el camión
freeCellsT truck

-- Cargar palet1 en el camión
let truck1 = loadT truck palet1

-- Verificar el estado del camión después de cargar palet1
truck1

-- Verificar las celdas libres después de cargar palet1
freeCellsT truck1

-- Cargar palet2 en el camión
let truck2 = loadT truck1 palet2

-- Verificar el estado del camión después de cargar palet2
truck2

-- Verificar las celdas libres después de cargar palet2
freeCellsT truck2

-- Cargar palet3 en el camión
let truck3 = loadT truck2 palet3

-- Verificar el estado del camión después de cargar palet3
truck3

-- Verificar las celdas libres después de cargar palet3
freeCellsT truck3

-- Cargar palet4 en el camión
let truck4 = loadT truck3 palet4

-- Verificar el estado del camión después de cargar palet4
truck4

-- Verificar las celdas libres después de cargar palet4
freeCellsT truck4

-- Descargar palets en "Cordoba"
let truckAfterUnload = unloadT truck4 "Cordoba"

-- Verificar el estado del camión después de descargar en "Cordoba"
truckAfterUnload

-- Verificar las celdas libres después de descargar en "Cordoba"
freeCellsT truckAfterUnload