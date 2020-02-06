module Intro 
    (maFonction
    , fToCelcius
    , Color (..)
    , showColor
    , isItRed
    , showPaymentType
    , PaymentType (..)
    , g
    , myhead)
    where

-- Functions
--             1        2       Result
maFonction :: Float -> Float -> Float 
maFonction monPremier monDeuxieme = monDeuxieme - monPremier 

type Farenheit = Float
type Celcius = Float 

fToCelcius :: Farenheit -> Celcius
fToCelcius f = f * 32.0  

-- Custom type (Enum)

data Color = RED | YELLOW | BLUE | BLACK

showColor :: Color -> String
showColor RED = "Its Red!" 
showColor YELLOW = "Its Yellow!"
showColor BLUE = "Its Blue!"
showColor BLACK = "Its Black!"

isItRed :: Color -> Bool
isItRed RED = True
isItRed _ = False 


data PaymentType = CASH | CREDITCARD | CHECK String

showPaymentType :: PaymentType -> String 
showPaymentType (CHECK no) = "it is a check " ++ no
showPaymentType _ = "It not a check" 

--data Maybe a = Nothing | Just a

g :: Maybe String -> String 
g Nothing = "Rien!!!!"
g (Just s) = "Jai ça : " ++ s

myhead :: [a] -> Maybe a 
myhead []  = Nothing 
myhead (x:_) = Just x