
siguiente :: Num a => a -> a
siguiente nro = nro + 1
calcular nro | even nro = siguiente nro
             | otherwise= doble nro

doble nro=nro*2



signo nro| nro== 0 = 0
         | nro < 0  = -1
         | otherwise = 1