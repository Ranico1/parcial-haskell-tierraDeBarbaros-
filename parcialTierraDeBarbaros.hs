import Data.Char (toUpper, isUpper)


data Barbaro = Barbaro {
nombre :: String,
fuerza :: Float,
habilidades :: [Habilidad],
objetos :: [Objeto]
}
type Objeto = Barbaro -> Barbaro 
type Habilidad = String 

-- 
espadas :: Float -> Barbaro -> Barbaro 
espadas peso = aumentarFuerza (cantidadAaumentar peso) 
    where cantidadAaumentar peso = peso / 4

aumentarFuerza :: Float -> Barbaro -> Barbaro
aumentarFuerza cantidadFuerza = mapFuerza (+ cantidadFuerza) 

mapFuerza :: (Float -> Float) -> Barbaro -> Barbaro 
mapFuerza f unBarbaro = unBarbaro {fuerza = f $ fuerza unBarbaro}

--

amuletosMisticos :: Habilidad -> Barbaro -> Barbaro
amuletosMisticos = agregarHabilidad  

agregarHabilidad :: Habilidad -> Barbaro -> Barbaro 
agregarHabilidad unaHabilidad = mapHabilidad (unaHabilidad :) 

mapHabilidad :: ([Habilidad] -> [Habilidad]) -> Barbaro -> Barbaro 
mapHabilidad f unBarbaro = unBarbaro {habilidades = f $ habilidades unBarbaro}

--  
varitasDefectuosas :: Barbaro -> Barbaro 
varitasDefectuosas = desaparecerObjetos.agregarHabilidad "hacer magia" 

desaparecerObjetos :: Barbaro -> Barbaro 
desaparecerObjetos unBarbaro = unBarbaro {objetos = []} 

-- 
unaArdilla :: Barbaro -> Barbaro 
unaArdilla unBarbaro = unBarbaro

--
cuerda :: Objeto -> Objeto -> Objeto  
cuerda unObjeto otroObjeto = unObjeto.otroObjeto 


--PUNTO 2 --

megafono :: Objeto 
megafono  = mapHabilidad (gritarHabilidades.concatenarHabilidades) 

concatenarHabilidades :: [String] -> [String]
concatenarHabilidades lista = [concat lista]

gritarHabilidades :: [String] -> [String]
gritarHabilidades = map (map toUpper) 

megafonoBarbarico :: Objeto 
megafonoBarbarico = cuerda unaArdilla megafono 


-- PUNTO 3 --
type Aventura = [Evento]
type Evento = Barbaro -> Bool 

-------------------------
aventuraLoca :: Aventura 
aventuraLoca = [sobrevivenAlaCremallera, sobrevivenAlosDuendes,sobrevivenRitualFechorias]
-------------------------

invasionDeSuciosDuendes :: [Barbaro] -> Bool 
invasionDeSuciosDuendes = all sobrevivenAlosDuendes 

sobrevivenAlosDuendes :: Barbaro -> Bool 
sobrevivenAlosDuendes unBarbaro =  "escribir poesia atroz" `elem` habilidades unBarbaro


cremalleraDelTiempo :: [Barbaro] -> Bool 
cremalleraDelTiempo = all sobrevivenAlaCremallera 

sobrevivenAlaCremallera :: Barbaro -> Bool 
sobrevivenAlaCremallera = tienePulgares 

tienePulgares :: Barbaro -> Bool 
tienePulgares unBarbaro = nombre unBarbaro == "Faffy" || nombre unBarbaro == "Astro"

ritualDeFechorias :: [Barbaro] -> Bool 
ritualDeFechorias grupoDebarbaros = all sobrevivenRitualFechorias grupoDebarbaros

sobrevivenRitualFechorias :: Barbaro -> Bool
sobrevivenRitualFechorias unBarbaro = esUnLadri unBarbaro || puedeGritar unBarbaro || sabeEscribirBien unBarbaro

esUnLadri :: Barbaro -> Bool
esUnLadri unBarbaro = "robar" `elem` habilidades unBarbaro && fuerza unBarbaro > 80

puedeGritar :: Barbaro -> Bool 
puedeGritar unBarbaro = gritoDeGuerra unBarbaro >= minimoDeAprobacion unBarbaro
    where minimoDeAprobacion unBarbaro = 4 * length (objetos unBarbaro)


gritoDeGuerra :: Barbaro -> Int
gritoDeGuerra = sum.map length.habilidades

caligrafia :: Barbaro -> Bool 
caligrafia unBarbaro = contienenMuchasVocales (habilidades unBarbaro) && all comienzanConMayuscula (habilidades unBarbaro)


comienzanConMayuscula :: Habilidad -> Bool 
comienzanConMayuscula habilidad = isUpper (head habilidad)

contienenMuchasVocales :: [Habilidad] -> Bool 
contienenMuchasVocales = all muchaCantidadDevocales 

muchaCantidadDevocales :: Habilidad -> Bool 
muchaCantidadDevocales = (>3).length.filter esVocal

esVocal :: Char -> Bool 
esVocal caracter =  caracter `elem` "aeiouAEIOU"

------------------------------------------------------------

sobrevivientes :: Aventura -> [Barbaro] -> [Barbaro]
sobrevivientes unaAventura grupoDeBarbaros = filter (sobrevivenAventura unaAventura) grupoDeBarbaros

sobrevivenAventura :: Aventura -> Barbaro -> Bool  
sobrevivenAventura unaAventura unBarbaro = all (\ evento -> evento unBarbaro) unaAventura
 
--- PUNTO 4 A 


