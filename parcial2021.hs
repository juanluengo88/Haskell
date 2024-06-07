data Auto = Auto{color::String,velocidad::Int,distanciaRecorrida::Int} deriving (Show, Eq)
type Carrera=[Auto]
pepe = Auto "Rojo" 100 1200
jose = Auto "Azul" 120 1500
roco= Auto "Verde" 90 1210

estanCerca::Auto->Auto->Bool
estanCerca auto1 auto2 = color auto1 /= color auto2 && abs(distanciaRecorrida auto1 - distanciaRecorrida auto2) < 10


tieneMasDistancia:: Eq Auto => Auto -> [Auto] -> Bool
tieneMasDistancia auto lista = all (\a -> (distanciaRecorrida auto) > distanciaRecorrida a ).filter (/= auto) $ lista

distinto::String->String->Bool
distinto str1 str2 = str1/=str2

vaTranquilo::Eq Auto => Auto->Carrera->Bool
vaTranquilo auto autos = any (not.estanCerca auto) autos && tieneMasDistancia auto autos

puesto::Auto->Carrera->Int
puesto auto autos = length(filter (\a -> distanciaRecorrida auto < distanciaRecorrida a) autos)+1

corra::Int->Auto->Auto
corra tiempo auto = auto{distanciaRecorrida= (distanciaRecorrida auto + (velocidad auto)*tiempo)}

type Modificador = Int->Int

modificarVelocidad::Auto->Modificador->Auto
modificarVelocidad auto f = auto{velocidad=f(velocidad auto)}

restar::Int->Modificador
restar num num1 = num1  - num

bajarVelocidad::Auto->Int->Auto
bajarVelocidad auto num | num < velocidad auto = modificarVelocidad auto (restar num)
                        | otherwise = modificarVelocidad auto (*0)


afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista


terremoto::Auto->Auto
terremoto auto = bajarVelocidad auto 50

miguelitos::Auto->[Auto]->Int->[Auto]
miguelitos auto autos num =  map (\auto->bajarVelocidad auto num) (filter (\a -> distanciaRecorrida auto > distanciaRecorrida a) autos) 

