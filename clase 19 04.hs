data Bebida = Cafe{ nombrebebida::String}|
                Gaseosa{sabor :: String, azucar :: Integer}

esEnergizante :: Bebida->Bool
esEnergizante (Cafe "capuchino") = True
esEnergizante(Gaseosa "pomelo" cantazucar)= cantazucar>10
esEnergizante _ = False

doble :: Num a => a -> a
doble nro=nro*2

pepe :: (a->Bool)->[a]->a
pepe condicion lista = (head.filter condicion) lista

data Politico = Politico {proyectosPresentados :: [String], sueldo :: Integer,  edadd :: Int } deriving Show

politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]

type Nombre = String
type Notas = [Int]
data Persona = Alumno {nombre :: Nombre, notas :: Notas}

