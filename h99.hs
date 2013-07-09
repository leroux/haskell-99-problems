module H99 where

import Prelude hiding (last, length, reverse, span)

myLast :: [a] -> a
myLast [] = error "last of empty list?!?"
myLast [x] = x
myLast (_:xs) = myLast xs

penultimate :: [a] -> a
penultimate [] = error "penultimate of empty list?!?"
penultimate [x, _] = x
penultimate (_:xs) = penultimate xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "out of list bounds."
elementAt (x:_) 0 = x
elementAt (_:xs) n = elementAt xs (n - 1)

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

length' :: [a] -> Int
length' = sum . map (const 1)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

data NestedList a = Elem a | List [NestedList a]

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress (nextDiff x xs)
  where nextDiff _ [] = []
        nextDiff x' (y:ys)
          | x' == y = nextDiff x' ys
          | otherwise = xs

span :: (a -> Bool) -> [a] -> ([a], [a])
span _ [] = ([], [])
span p (x:xs)
  | p x = x : span p xs', xs')
  | otherwise = ()