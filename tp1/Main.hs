import Control.Exception
import System.IO.Unsafe
import Palet
import Stack
import Truck
import Route

-- Función para testear si una operación falla
testF :: Show a => a -> Bool
testF action = unsafePerformIO $ do
    result <- tryJust isException (evaluate action)
    return $ case result of
        Left _ -> True
        Right _ -> False
    where
        isException :: SomeException -> Maybe ()
        isException _ = Just ()



-- Definiciones para testear
paletB = newP "CiudadB" 3
paletA = newP "CiudadA" 4
paletC = newP "CiudadC" 2
paletPesado = newP "CiudadC" 50  -- Excede el peso permitido del truck
paletOutRoute = newP "CiudadE" 3  -- Ciudad destino fuera de la ruta

route = newR ["CiudadA", "CiudadB", "CiudadC", "CiudadD"]
routeRep = newR ["CiudadA", "CiudadB", "CiudadC", "CiudadD", "CiudadB"]

truck1 = newT 2 10 route  -- Truck con 2 Stacks de altura 10 cada uno
truckFull = newT 2 1 route  
truck2 = newT 1 5 route  
truck3 = newT 2 2 route

truckFull2 = loadT truckFull paletB
truckFull3 = loadT truckFull2 paletA

truck4 = loadT truck3 paletA
truckNotHoldsC = loadT truck4 paletB

truckTap0 = newT 1 2 route -- este truck va a quedar con palets tapados por otros cuando los querramos descargar
truckTap1 = loadT truckTap0 paletB
truckTapado = loadT truckTap1 paletA


-- Tests
runTests :: [Bool]
runTests = 
    [ 
      -- 1. Destino de palet
      destinationP paletB == "CiudadB"

      -- 2. Peso de palet
    , netP paletB == 3 

      -- 3. Crear instancia de ruta con lista vacía
    , testF (newR [])

      -- 4. Crear instancia de ruta con lista de ciudades
    , not $ testF (newR ["CiudadA", "CiudadB", "CiudadC"])

      -- 5. Ciudad 1 está antes que Ciudad 2 en la ruta
    , inOrderR route "CiudadA" "CiudadC" == True

      -- 6. Ciudad 1 no está antes que Ciudad 2 en la ruta
    , inOrderR route "CiudadC" "CiudadA" == False

      -- 7. Ciudad 1 no está antes que Ciudad 1 en la ruta
    , inOrderR route "CiudadB" "CiudadB" == False

      -- 8. Ciudad 1 (que está repetida luego) está antes que Ciudad 2 en la ruta repetida
    , inOrderR routeRep "CiudadB" "CiudadD" == True   -- Asumimos que cada ciudad está en la posición que se cargó primero

      -- 9. Creación de Truck
    , freeCellsT truck1 == 20

      -- 10. Se permite subir un palet a un stack disponible
    , not $ testF (loadT truck1 paletB)

      -- 11. No se puede subir palet porque excede peso de la bahía
    , testF (loadT truck1 paletPesado)

      -- 12. No se puede subir palet porque todos los stacks están llenos
    , testF (loadT truckFull3 paletB)

      -- 13. No se puede subir palet porque no hay stack que lo acepte (por orden de ciudades)
    , testF (loadT truckNotHoldsC paletC)

      -- 14. No se puede cargar un palet que tiene destino fuera de la ruta
    , testF (loadT truck2 paletOutRoute)

      -- 15. Saltearse la descarga en una ciudad, y descargar los de otra que los tiene tapados (descargar en B antes que en A)
    , netT (unloadT truckTapado "CiudadB") == netT (truckTapado)  -- si está tapado, no se puede descargar (por lo tanto el camión sigue igual)

      -- 16. Descargo igual que antes ("salteándome una ciudad"), pero sin estar tapado el palet de la ciudadB
    , netT (unloadT truckFull3 "CiudadB") /= netT (truckFull3)  -- si no está tapado, se puede descargar (por lo tanto el camión queda con un palet menos)

      -- 17. Descargar un palet en una ciudad que no está en la ruta
    , netT (unloadT truckFull3 "CiudadE") == netT (truckFull3)  -- si la ciudad no está en la ruta el camión sigue igual
    ]


main :: IO ()
main = print runTests
