class Describible where
    description :: a -> String


data Color = Rojo
            | Azul
            | Verde

instance Describible Color where
    description (Rojo) = "El color es rojo"
    description (Azul) = "El color es azul"
    description (Verde) = "El color es verde"

data Color = Rojo | Verde | Azul
    deriving (Show, Eq, Ord) --muestra con show, eq es equals y ord compara cual va a ntes

----------------------------------------------------------------------------

-- Data crea un nuevo tipo de dato, class como se comporta, instance iuna instancia que utiliza e implementa class

-- Deriving es una implementacion automatica, genera el comportamiento ya definido, instance implementa manualmnte, osea personalizado

data TipoNumeros = Positivo 
                | Negativo
                | Cero
    deriving (Show,Eq,Ord)

-----------------------------------------------------------------------------

data Estacion = Verano | Otono | Invierno | Primavera

instance Eq Estacion where
    Verano == Verano True
    Otono == Otono True
    Invierno == Invierno True
    Primavera == Primavera True
    _ == _ False  -- _ significa todo el resto de combinaciones, simepre para al final porque se evalua en orden de arriba abajo

-------------------------------------------------------------------------------


multiplosDeTres :: [Integer]

multiplosDeTres = [3,6,...] -- el ... genera una lista infinita siguiente el patr0on anterior

take 10 multiplosDeTres -- toma los pimeros 10 elementos de la ista

--------------------------------------------------------------------------------

