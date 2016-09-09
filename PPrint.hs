module PPrint (pprint) where
import Data.List
import Data.Char
import Data.Ratio

connect :: [[[a]]] -> [[a]] 
connect = map concat . transpose

bigSign, mono :: Rational -> Bool -> [String]
bigSign ratio isFirst = case signum (numerator ratio) of {
  -1 -> ["   ", " - ", "   "]; 
   1 -> let x = if isFirst then "   " else " + " in ["   ", x, "   "]
}
mono ratio isPositive = 
    if d == "1" 
    then if n == "1" && isPositive
         then [] 
         else [replicate l ' ', n, replicate l ' ']
    else [n, replicate l '-', d]
      where n = show (abs $ numerator ratio)
            d = show (denominator ratio)
            l = max (length n) (length d)

super :: Int -> [String]
super 0 = []
super 1 = [" ", "x", " "]
super n = ["  ", 'x':(map (("⁰¹²³⁴⁵⁶⁷⁸⁹" !!) . digitToInt) $ show n), "  "]

threesome :: Bool -> (Rational, Int) -> [String]
threesome isFirst (ratio, power) =
    if signum (numerator ratio) == 0
    then []
    else connect [bigSign ratio isFirst, mono ratio (power > 0), super power]

pprint :: [Rational] -> Bool -> String
pprint xs asc = unlines . connect . threes $ dropWhile (\x -> fst x == 0) s
    where s = if asc
              then zip xs [0..]
              else zip (reverse xs) [length xs - 1, length xs - 2..]
          threes [] = []
          threes (t:ts) = threesome True t : (map (threesome False) ts) 
