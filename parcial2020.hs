data Jugador = UnJugador {nombre :: String,padre :: String,habilidad :: Habilidad} deriving (Eq, Show)

data Habilidad = Habilidad {fuerzaJugador :: Double,precisionJugador :: Double} deriving (Eq, Show)


data Tiro = UnTiro {velocidad :: Double,precision ::   Double,altura :: Double} deriving (Eq, Show)

type Puntos = Int



bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b | f a > f b = a
                 | otherwise = b

maximoSegun :: (Foldable t, Ord a1) => (a2 -> a1) -> t a2 -> a2
maximoSegun f = foldl1 (mayorSegun f)



putter::Habilidad->Tiro
putter habilidad = UnTiro{velocidad=10,precision =(precisionJugador habilidad)*2 , altura=0}

madera::Habilidad->Tiro
madera habilidad = UnTiro{velocidad=100,precision=precisionJugador habilidad/2, altura=5}

restar nro1 | nro1>=3 = nro1 -3
            | otherwise = 0


hierros::Double->Habilidad->Tiro
hierros  nro habilidad= UnTiro {velocidad=fuerzaJugador habilidad*nro,precision=precisionJugador habilidad/nro,altura=restar nro}

type Palo=Habilidad->Tiro
type Palos =[Palo]

palos::[Palo]
palos=[putter,madera ] ++ map hierros [1..10]





