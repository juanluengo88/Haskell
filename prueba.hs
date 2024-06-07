type Bien = (String,Float)	
type Ciudad=[Ciudadano]
data Ciudadano = UnCiudadano {profesion :: String, sueldo :: Float, 
cantidadDeHijos :: Float, bienes :: [Bien] } deriving Show


homero :: Ciudadano
homero = UnCiudadano "SeguridadNuclear" 9000 3 [("casa",50000), ("deuda",-70000)]
frink :: Ciudadano
frink = UnCiudadano "Profesor" 12000 1 []
krabappel :: Ciudadano
krabappel = UnCiudadano "Profesor" 12000 0 [("casa",35000)]
burns :: Ciudadano
burns = UnCiudadano "Empresario" 300000 1 [("empresa",1000000),("empresa",500000),("auto",200000)]


springfield :: [Ciudadano]
springfield = [homero, burns, frink, krabappel]

caca::Ciudad
caca=[homero,burns]

diferenciaDePatrimonio::Ciudad->Float
diferenciaDePatrimonio ciudad= (patrimonio.ciudadanoSegun mayorPatrimonio) ciudad - (patrimonio.ciudadanoSegun menorPatrimonio) ciudad


patrimonio::Ciudadano->Float
patrimonio ciudadano = foldl (\sem (_,bien)->sem+bien) (sueldo ciudadano) (bienes ciudadano)

ciudadanoSegun::(Ciudadano -> Ciudadano->Ciudadano) -> Ciudad -> Ciudadano
ciudadanoSegun f (ciudadano:ciudadanos) = foldl f ciudadano ciudadanos

mayorPatrimonio::Ciudadano -> Ciudadano ->Ciudadano
mayorPatrimonio unapersona otrapersona | patrimonio unapersona > patrimonio otrapersona = unapersona
                                       | otherwise = otrapersona

menorPatrimonio::Ciudadano -> Ciudadano ->Ciudadano
menorPatrimonio unapersona otrapersona | patrimonio unapersona < patrimonio otrapersona = unapersona
                                       | otherwise = otrapersona


tieneAutoCaro::Ciudadano->Bool
tieneAutoCaro persona = (any (("auto"==).fst).bienes $ persona) && (any ((100000<=).snd).bienes $ persona)

type Medida = Ciudadano->Ciudadano


--auh :: Ciudadano -> Ciudadano
--auh ciudadano = aplicarMedidaSegun (patrimonio ciudadano < 0) (modificarSueldo (incremento.cantidadDeHijos)ciudadano) ciudadano 

aplicarMedidaSegun::Bool->(Ciudadano->Ciudadano)->Ciudadano->Ciudadano
aplicarMedidaSegun condicion f ciudadano | condicion = f ciudadano
                                         |otherwise = ciudadano


modificarSueldo::Float->Ciudadano->Ciudadano
modificarSueldo cantidad ciudadano= ciudadano{sueldo=sueldo ciudadano + cantidad}


incremento::Float->Float
incremento cantHijos = 1000*cantHijos


impuestoGanancias::Float->Medida
impuestoGanancias minimo ciudadano = aplicarMedidaSegun (sueldo ciudadano > minimo) (modificarSueldo (diferencia minimo (sueldo ciudadano)))ciudadano

diferencia::Float->Float->Float
diferencia num num2 = (num-num2)*0.3


f1 x y z = map (*x) . filter (y z) 
