type Nombre = String
type Alcohol = Int

data Trago = CTrago Nombre Alcohol deriving (Show, Eq)

nombre :: Trago -> Nombre
nombre (CTrago nombre _) = nombre
alcohol :: Trago -> Alcohol
alcohol (CTrago _ alcohol) = alcohol

esRico :: Trago -> Bool
esRico trago = 
    tieneMasDe10Letras trago
    || esDaiquiri (nombre trago)

-- esFuerte trago = alcohol trago > 10
esFuerte :: Trago -> Bool
esFuerte = mayorA10. alcohol

mayorA10 :: Int -> Bool
mayorA10 n = n > 10

-- tieneMasDe10Letras trago = length trago > 10
tieneMasDe10Letras :: Trago -> Bool
tieneMasDe10Letras = mayorA10 . length . nombre

esDaiquiri :: String -> Bool
esDaiquiri "Daiquiri" = True
esDaiquiri _ = False


type Barman = (Nombre, Edad)
type Edad = Int

edad :: Barman -> Edad
edad (_, edad) = edad

nombreBarman :: Barman -> Nombre
nombreBarman (nombre, _) = nombre

type Herramienta = Barman -> Trago


licuadora :: Herramienta
licuadora barman = CTrago "Daiquiri" (edadDiv10 barman)

coctelera :: Herramienta
coctelera barman = CTrago "Gancia" (cantDeLetras barman)
 
coca :: Int -> Herramienta
coca hielos barman = CTrago "Fernet con coca" (edad barman - hielos)








cantDeLetras = length . nombreBarman

edadDiv10 barman = div (edad barman) 10



emi :: Barman
emi = ("Emiliano", 25)








daiquiri :: Trago
daiquiri = CTrago "Daiquiri" 7











