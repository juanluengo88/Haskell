pesopino :: Int->Int
pesopino nro | nro<=3 = nro*100*3
             | otherwise = nro*100*2

esPesoUtil:: Int -> Bool
esPesoUtil nro = 400<=nro && nro<=1000 

sirvepino :: Int-> Bool
sirvepino = esPesoUtil.pesopino


inversa :: Float -> Float
inversa nro | nro /= 0 =  1/nro 
            | otherwise = 0

mitad:: Float -> Float
mitad nro = nro/2 

sumalista::[Int]->Int
sumalista [] = 0
sumalista (x:xs) = x + sumalista(xs) 

promedio::[Int]->Float
promedio lista = (convertirAFloat.sumalista) lista / (convertirAFloat.length) lista 

convertirAFloat :: Int -> Float
convertirAFloat n = fromIntegral n :: Float

convertirListaAFloat :: [Int] -> [Float]
convertirListaAFloat  = map fromIntegral 

cantelementos::[(Int,Int)]->Int
cantelementos lista = foldl (\sem _ -> sem + 1 ) 0 lista 

cantelementos'::[(Int,Int)]->Int
cantelementos' lista = foldr (\ _  sem -> sem + 1 ) 0 lista 

masgastador::[(String,Integer)]->(String,Integer)
masgastador (cab:cola) = foldl mayorgasto cab cola

mayorgasto::(String,Integer)->(String,Integer)->(String,Integer)
mayorgasto emple1 emple2 | snd emple1 > snd emple2 = emple1
                         | otherwise = emple2

totalgasto lista = foldl (\ sem (_,gasto)-> sem + gasto)  0  lista 

totalgasto' lista =foldr(\(_,gasto) sem -> gasto + sem) 0 lista 


type Nombre  = String
type InversionInicial = Int
type Profesionales = [String]

data  Proyecto = Proy {nombre:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show
proyectos = [Proy "red social de arte"  20000 ["ing. en sistemas", "contador"],
            Proy "restaurante" 5000 ["cocinero", "adm. de empresas", "contador"],
            Proy "ventaChurros" 1000 ["cocinero"] ]

maxproysegun::(Proyecto->Int)->[Proyecto]->Proyecto
maxproysegun f (cab:cola) = foldl  (mayorinversion f) cab cola

mayorinversion :: (Proyecto -> Int) -> Proyecto-> Proyecto -> Proyecto
mayorinversion f proy1 proy2 | f proy1 > f proy2 = proy1
                             | otherwise = proy2 

maxproysegun'::(Proyecto->Int)->[Proyecto]->Proyecto
maxproysegun' f (cab:cola) = foldr  (mayorinversion f) cab cola

maxproysegun''::(Proyecto->Int)->[Proyecto]->Proyecto
maxproysegun'' f lista = foldl1  (mayorinversion f) lista


f :: (a1 -> Bool) -> (a2 -> a1) -> Int -> [a2] -> Bool
f x y z lista = ((> z). length . filter x. map y) lista


h :: Eq b => b -> [(a, b, c)] -> (a, b, c)
h nom   = head.filter ((nom==).g) 


g :: (a, b, c) -> b
g(_,c,_)=c


t :: (Ord a, Num a, Num t) => t -> (t -> a) -> [t] -> t
t x _ [] = x                                                --caso base
t x y (z:zs)| y z > 0 = z + t x y zs                        --caso recursivo 
            | otherwise = t x y zs  

