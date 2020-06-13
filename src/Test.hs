module test where

data Color = RED | BLUE | GREEN
data PaymentMethod = CASH | CB | Check String

data Person = 
    Person {
             name :: String,
             surname :: String,
             age :: Int
           }

f :: PaymentMethod -> String
f CASH = "cash"
f CB = "cb"
f (Check s) = "check number " ++ s 


combine :: Color -> (Color -> Color)
combine BLUE YELLOW = GREEN 
combine c1 c2 = case (c1, c2) of
                   (BLUE, YELLOW) -> GREEN
                   (BLUE, BLUE) -> BLUE

fmap :: Functor f => (a -> b) -> f a -> f b 
fmap :: (a -> b) -> [a] -> [b]
fmap :: (a -> b) -> Maybe a -> Maybe b 

data Maybe a = Nothing | Just a 
