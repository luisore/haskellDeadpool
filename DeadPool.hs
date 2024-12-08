import Text.Show.Functions

data Mutante = Mutante {
nombre :: String,
vida :: Float,
habilidades :: [String],
armas :: [Arma]
}deriving (Show)

type Arma = Mutante->Mutante

deadpool = Mutante "Deadpool" 150 ["comer","cagar","respirar"] [punio 5,espadas 10]
spiderman = Mutante "SpiderMan" 125 ["Esquivar Golpes","telarania","saltar","atrapar"] [punio 5,espadas 5, pistola 15]
wolverine = Mutante "Wolverine" 200 ["regenerarVida","inmunidad","resistencia"] [espadas 10, espadas 10, punio 5]
antman = Mutante "Ant-Man" 100 ["Esquivar Golpes","achicarse","correr","distraer"] [pistola 15,punio 5]
thor = Mutante "Thor" 175 ["saltoAlto","levantarMartillo","relampago"] [martilloThor,punio 5]
blackwidow = Mutante "BlackWidow" 100 ["Esquivar Golpes","mostrar el Orto","Seducir"] [widowBite,pistola 10,punio 5,espadas 15]
hulk = Mutante "Hulk" 500 ["Tanquear","Enojarse"] [punio 50]
coloso = Mutante "Coloso" 250 ["Sesgometalico","Ruidometalico"] [punio 250]


doctorDoom = Mutante "Dr. Doom" 200 ["CopiarArma","DOOOOMMM!!!","HabilidadOculta","MiradaDeMalo"] [doom,doom,doom,doom,doom]


--Extra shiet
martilloThor victima =(Mutante (nombre victima) (max 0(subtract 33(vida victima))) (habilidades victima) (drop 2 (armas victima)))
widowBite victima = (Mutante (nombre victima) (max 0(subtract 10(vida victima))) (map reverse (habilidades victima)) (tail (armas victima)))
doom victima = (Mutante (nombre victima) (max 0(vida victima -10)) (["DOOOM","DOOOOOMM!!","DOOOOOOMED!!!!"]) [silenced])

----------------- DR DOOM OP NERF PLS
atacarConTodo atacante (Mutante "Dr. Doom" vidaDoom habilidadesDoom armasDoom) = aumentarvida (100) (cargarElBolso [((head.armas) atacante)] (foldr ($) (Mutante "Dr. Doom" vidaDoom habilidadesDoom armasDoom) (armas atacante)))
atacarConTodo atacante victima = foldr ($) (victima) (armas atacante)

--GG INVENCIBLE
--rescatarAMiChica [blackwidow,spiderman,coloso,hulk,wolverine,antman,thor] [doctorDoom]

--Contratacar DOOM
--atacarConTodoYBancarseElContraataque [blackwidow,spiderman,coloso,hulk,wolverine,antman,thor] doctorDoom

--Thor salva el dia desarmando a Dr DOOM
--atacarConTodoYBancarseElContraataque [thor,blackwidow,spiderman,coloso,hulk,wolverine,antman] doctorDoom 

atacarConTodoYBancarseElContraataque amigos victima = victima : map (atacarConTodo (todosAUno amigos victima)) (amigos)


aumentarvida cantidad victima = (Mutante (nombre victima) (max 0(vida victima + cantidad))(habilidades victima)(armas victima))
silenced mutante = mutante


--1
estaMuerto = (==0).vida
--2
esFrancis = ("Francis"==).nombre
--3
isMyDad mutante = nombre mutante == "Coloso" || all esMetalica (habilidades mutante)
esMetalica = any ("metal"==).words
--4
punio potencia victima	|(elem "Esquivar Golpes".habilidades)victima = victima
				|otherwise = reducirvida potencia victima
				

				
reducirvida cantidad victima = (Mutante (nombre victima) (max 0(vida victima - cantidad))(habilidades victima)(armas victima))
--5----------------------------------------------Use fromIntegral to convert the Ints to Float
pistola calibre victima = reducirvida (((*calibre).fromIntegral.length.habilidades) victima) victima
--6
espadas fuerza = reducirvida (fuerza/2)
--7
cargarElBolso _ (Mutante "Deadpool" vidaDead habilidadesDead armasDead)=(Mutante "Deadpool" vidaDead habilidadesDead [pistola 9, espadas 10])
cargarElBolso nuevasArmas mutante = (Mutante (nombre mutante) (vida mutante) (habilidades mutante) (armas mutante ++ nuevasArmas))
--8
ataqueRapido atacante victima = (head.armas) atacante victima
--9

--ejm atacarConTodo spiderman deadpool
--     (espadas 7)$victima
--10
todosAUno mutantes victima = foldr atacarConTodo victima mutantes
----------------------------------------(atacarConTodo) spiderman hulk
--todosAUno [deadpool,spiderman,wolverine,antman,thor,blackwidow] hulk
--				10   ,  37.5   ,   15    ,  35  , 38 , 42.5
--178 damage
--11
comoEstaSuFamilia atacante victimas = map (nombre) (filter (estaMuerto.ataqueRapido atacante) victimas)
--comoEstaSuFamilia coloso [deadpool,spiderman,wolverine,antman,thor,blackwidow,hulk] 
--["Deadpool","SpiderMan","Wolverine","Ant-Man","Thor"]
--12
rescatarAMiChica amigos victimas = filter (not.estaMuerto) (map (todosAUno amigos) (map (ataqueRapido deadpool) (filter (not.estaMuerto.ataqueRapido deadpool) victimas)))
--rescatarAMiChica [blackwidow,spiderman] [wolverine,antman,thor,hulk,coloso]
--	Wolverine Thor Hulk Coloso

--13
cualEsMiNombre x y z = concat.map x.filter ((y>).vida)
-- cualEsMiNombre
-- :: (Mutante -> [a]) -> Float -> t -> [Mutante] -> [a]















 
 
 