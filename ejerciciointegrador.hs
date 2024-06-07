data Animal= Raton {nombre :: String, edad :: Double, peso :: Double,
 enfermedades :: [String]} deriving Show

cerebro :: Animal
cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]


modificarEdad::(Double->Double)->Animal->Animal
modificarEdad f animal = animal{edad = (f.edad)animal} 

modificarNombre::(String->String)->Animal->Animal
modificarNombre f animal = animal {nombre = (f.nombre)animal}

modificarPeso::(Double->Double)->Animal->Animal
modificarPeso f animal = animal {peso = (f.peso)animal}


modificarEnfermedades :: ([String] -> [String]) -> Animal -> Animal
modificarEnfermedades f animal = animal {enfermedades = (f.enfermedades)animal}

hierbaBuena::Animal->Animal
hierbaBuena animal = modificarEdad sqrt animal 

hierbaVerde ::String->Animal->Animal 
hierbaVerde enfermedad animal = modificarEnfermedades (filter(/=enfermedad)) animal 

alcachofa::Animal->Animal
alcachofa animal = modificarPeso descuentoPeso  animal
            

descuentoPeso :: Double -> Double
descuentoPeso peso | peso > 2   = peso * 0.9
                   | otherwise  = peso * 0.95
    
                   
medicamento :: [(Animal -> Animal)] -> Animal -> Animal
medicamento hierbas raton = foldl (flip ($))  raton   hierbas

medicamento' :: [(Animal -> Animal)] -> Animal -> Animal
medicamento' hierbas raton = foldl (\unRaton unaHierba -> unaHierba unRaton) raton hierbas


antiAge :: Animal -> Animal
antiAge raton= medicamento (replicate 3 hierbaBuena ++ [alcachofa]) raton 

