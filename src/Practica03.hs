module Practica03 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
w = Var "w"
v = Var "v"

{-
FORMAS NORMALES
-}

--Ejercicio 1

{-Función para quitar implicaciones y doble implicaciones -}
quitaImpl :: Prop -> Prop
quitaImpl (Cons True)  = Cons True
quitaImpl (Cons False) = Cons False
quitaImpl (Var p)      = Var p
quitaImpl (Not p)      = Not (quitaImpl p)
quitaImpl (Or p q)     = Or (quitaImpl p) (quitaImpl q)
quitaImpl (And p q)    = And (quitaImpl p) (quitaImpl q)
quitaImpl (Impl p q)   = Or (Not (quitaImpl p)) (quitaImpl q)
quitaImpl (Syss p q)   = And (Or (Not (quitaImpl p)) (quitaImpl q)) 
                             (Or (Not (quitaImpl q)) (quitaImpl p))

{- Función que maneja los casos de la negación para la forma normal negativa -}
casoNegacion :: Prop -> Prop
casoNegacion (Cons True)  = Cons False
casoNegacion (Cons False) = Cons True
casoNegacion (Var p)      = Not (Var p)
casoNegacion (Not p)      = fnnAux p
casoNegacion (Or p q)     = And ( fnnAux (Not p)) (fnnAux (Not q))
casoNegacion (And p q)     = Or ( fnnAux (Not p)) (fnnAux (Not q)) 

{-Función auxiliar para la forma normal neagtiva, solo recibe fórmulas
sin implicaciones ni doble implicaciones -}
fnnAux :: Prop -> Prop
fnnAux (Cons True)  = Cons True
fnnAux (Cons False) = Cons False
fnnAux (Var p)      = Var p
fnnAux (Not p)      = casoNegacion p
fnnAux (Or p q)     = Or (fnnAux p) (fnnAux q)
fnnAux (And p q)    = And (fnnAux p) (fnnAux q)

fnn :: Prop -> Prop
fnn p = fnnAux (quitaImpl p)


--Ejercicio 2

distribuir :: Prop -> Prop -> Prop
distribuir (Var p) (Var q) = Or (Var p) (Var q)
distribuir (Not (Var p)) (Var q) = Or (Not (Var p)) (Var q)
distribuir (Var p) (Not (Var q)) = Or (Var p) (Not (Var q))
distribuir (Not (Var p)) (Not (Var q)) = Or (Not (Var p)) (Not (Var q))
distribuir (And p q) r = And (fncAux (Or p r)) (fncAux (Or q r))
distribuir p (And q r) = And (fncAux (Or p q)) (fncAux (Or p r))
distribuir (Or p q) r = Or (fncAux(Or p q)) (fncAux r)
distribuir p (Or q r) = Or (fncAux p) (fncAux(Or q r))


fncAux :: Prop -> Prop
fncAux (Cons True) = Cons True
fncAux (Cons False) = Cons False
fncAux (Var p) = (Var p)
fncAux (Not (Var p)) = Not (Var p)
fncAux (And p q) = And (fncAux p) (fncAux q)
fncAux (Or p q) = distribuir (fncAux p) (fncAux q)


fnc :: Prop -> Prop
fnc p = fncAux(fnn p)

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas = undefined

--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion = undefined

{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente = undefined

--Ejercicio 2
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion = undefined