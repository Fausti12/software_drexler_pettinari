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
    , testF (loadT truckFull palet1)

      -- Test 4: Todos los stacks están llenos
    , testF (loadT truckFull palet2)

        -- Test 5: No se puede cargar un palet que tiene destino fuera de la ruta
    , testF (loadT truck2 paletOutRoute)
    ]

-- Definiciones para testear
palet1 = newP "CiudadB" 3
palet2 = newP "CiudadA" 4
paletPesado = newP "CiudadC" 50  -- Excede el peso permitido del truck
paletOutRoute = newP "CiudadD" 3  -- Ciudad destino fuera de la ruta

route = newR ["CiudadA", "CiudadB", "CiudadC"]

truck1 = newT 2 10 route  -- Truck con 2 Stacks de altura 10 cada uno
truckFull = foldl loadT truck1 (replicate 2 palet1)  -- Truck lleno
truck2 = newT 1 5 route  

main :: IO ()
main = print runTests
