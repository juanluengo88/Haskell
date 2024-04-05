
siguiente :: Num a => a -> a
siguiente nro = nro + 1
calcular nro | even nro = siguiente nro
             | otherwise= doble nro

doble :: Num a => a -> a
doble nro=nro*2



signo :: (Num a1, Num a2, Ord a1) => a1 -> a2
signo nro| nro== 0 = 0
         | nro < 0  = -1
         | otherwise = 1

aprobo :: (Ord a, Num a) => a -> Bool
aprobo nota = nota >= 6 

mes::(interger,interger,interger) -> interger
mes (_,m,_)=m
