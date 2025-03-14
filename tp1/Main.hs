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