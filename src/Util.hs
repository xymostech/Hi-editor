module Util where

modifyPosition :: Int -> (a -> a) -> [a] -> [a]
modifyPosition _ _ [] = []
modifyPosition 0 f (x:xs) = (f x) : xs
modifyPosition pos f (x:xs) = x : (modifyPosition (pos - 1) f xs)

replacePosition :: Int -> a -> [a] -> [a]
replacePosition _ _ [] = []
replacePosition 0 y (x:xs) = y : xs
replacePosition pos y (x:xs) = x : (replacePosition (pos - 1) y xs)

insertAtPosition :: Int -> a -> [a] -> [a]
insertAtPosition 0 elem list = elem : list
insertAtPosition _ _ [] = []
insertAtPosition pos elem (x:xs) = x : (insertAtPosition (pos - 1) elem xs)

removeAtPosition :: Int -> [a] -> [a]
removeAtPosition _ [] = []
removeAtPosition 0 list = list
removeAtPosition 1 (x:xs) = xs
removeAtPosition pos (x:xs) = x : (removeAtPosition (pos - 1) xs)
