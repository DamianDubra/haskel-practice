class Describible where
    description :: a -> String


data Color = Rojo
            | Azul
            | Verde

instance Describible Color where
    description (Rojo) = "El color es rojo"
    description (Azul) = "El color es azul"
    description (Verde) = "El color es verde"