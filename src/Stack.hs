module Stack 
	where
import Data.Maybe

data Stack a = Stack [a] 
 	      | Empty
             deriving(Show)

instance Eq (Stack a) where
      Empty == Empty = True
      _     == Empty = False

push :: a -> Stack a -> Stack a
push x Empty = Stack [x]
push x (Stack xs) = Stack (x:xs)

popfrom :: Stack a -> (Maybe a,Stack a)
popfrom Empty = (Nothing,Empty)
popfrom (Stack (x:xs)) = (Just x,Stack xs)

lenofStack :: Stack a -> Int
lenofStack Empty = 0
lenofStack (Stack []) = 0
lenofStack (Stack (x:xs)) = 1 + lenofStack (Stack xs)

pop :: Stack a -> Maybe a
pop st = fst (popfrom st)

remainStack :: Stack a -> Stack a
remainStack st = snd (popfrom st)

isEmpty :: Stack a -> Bool
isEmpty st1
         | st1 == Empty = True
         | otherwise = False
peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:xs)) = Just x

