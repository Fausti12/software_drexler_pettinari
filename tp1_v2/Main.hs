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

-- Tests
runTests :: [Bool]
runTests = 
    [ -- Test 1: Se permite subir un palet a un stack disponible
      not $ testF (loadT truck1 palet1)

      -- Test 2: No se puede subir palet porque excede peso del Truck
    , testF (loadT truck1 paletPesado)

      -- Test 3: No se puede subir palet porque no hay stack disponible
    , testF (loadT truckFull3 palet1)

      -- Test 4: Todos los stacks están llenos
    , testF (loadT truckFull3 palet2)

      -- Test 5: No se puede cargar un palet que tiene destino fuera de la ruta
    , testF (loadT truck2 paletOutRoute)

      -- Test 6: Saltearse la descarga en una ciudad, y descargar los de otra que los tiene tapados (descargar en B antes que en A)
    , unloadT truckTapado "CiudadB" == truckTapado  -- si está tapado, no se puede descargar; por lo tanto el camión sigue igual

      -- Test 7: Descargo igual que antes ("salteándome una ciudad"), pero sin estar tapado el palet de la ciudadB
    , unloadT truckFull3 "CiudadB" /= truckFull3  -- si no está tapado, se puede descargar; por lo tanto el camión queda con un palet menos

    ]

-- Definiciones para testear
palet1 = newP "CiudadB" 3
palet2 = newP "CiudadA" 4
paletPesado = newP "CiudadC" 50  -- Excede el peso permitido del truck
paletOutRoute = newP "CiudadD" 3  -- Ciudad destino fuera de la ruta

route = newR ["CiudadA", "CiudadB", "CiudadC"]

truck1 = newT 2 10 route  -- Truck con 2 Stacks de altura 10 cada uno
truckFull = newT 2 1 route  -- Truck con 2 Stacks de altura 2 cada uno
truck2 = newT 1 5 route  

truckFull2 = loadT truckFull palet1
truckFull3 = loadT truckFull2 palet2

truckTap0 = newT 1 2 route -- este truck va a quedar con palets tapados por otros cuando los querramos descargar
truckTap1 = loadT truckTap0 palet1
truckTapado = loadT truckTap1 palet2

main :: IO ()
main = print runTests
