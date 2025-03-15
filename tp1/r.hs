-- itero los stacks y si uno tiene una ciudad que va después de la de mi palet o no tiene nada (chequeco con holdsS), apilo en ese stack (con StackS)
-- iterateStacks [] pal _ = [pal] -- si no hay stacks, devuelvo lista con el palet
iterateStacks stackL pal rou  | null stackL = [] -- si no hay stacks, devuelvo lista vacía
                              | (netP pal + netS (head stackL)) > 10 = [head stackL] ++ iterateStacks (tail stackL) pal rou -- si el palet pesa más de 10 toneladas, no lo cargo
                              | holdsS (head stackL) pal rou = [stackS (head stackL) pal] ++ (tail stackL) -- si holdsS es True, apilo en stack y devuelvo lista con ese stack y el resto
                              | otherwise = [head stackL] ++ iterateStacks (tail stackL) pal rou -- si holdsS es False, devuelvo lista con el stack y sigo iterando

-- EL problema es que se puede hacer head/tail de algo nulo 
