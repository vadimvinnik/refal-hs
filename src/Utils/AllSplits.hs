module Utils.AllSplits (allSplits) where

allSplits :: [a] -> [([a], [a])]
allSplits [] = [([], [])]
allSplits l@(x:xs) = ([], l) : (map (\(p, q) -> (x:p, q)) $ allSplits xs)
