module Base (Poly, factors, fromDescList, degree, doubleton, showAsc, showDesc) where
import Data.Ratio
import PPrint

type Rats = [Rational]
newtype Poly = Poly Rats
data DivResult = DivByZero | Quotient Poly | QuotRem Poly Poly

-- for GHCi
print' :: Poly -> IO()
print' = putStrLn . show
expand :: [Rats] -> Poly
expand = product . map fdl

instance Show DivResult where
    show DivByZero     = "ERROR: Divide by zero (empty Poly)"
    show (Quotient q)  = "Quotient\n" ++ show q
    show (QuotRem q r) = "Quotient\n" ++ show q ++ "Remainder\n" ++ show r

instance Show Poly where
    show = showDesc

instance Eq Poly where
    Poly xs == Poly ys = xs == ys
    Poly xs /= Poly ys = xs /= ys

instance Num Poly where
    negate (Poly xs)    = Poly (map negate xs)
    signum (Poly [])    = Poly []
    signum (Poly (x:_)) = Poly [signum x]
    abs (Poly [])       = Poly []
    abs (Poly xs)       = Poly (map (/ (last xs)) xs)
    fromInteger integer = Poly [fromIntegral integer]
    px - py             = px + (negate py)
    Poly xs + Poly ys   = Poly (xs `add` ys)
    Poly xs * Poly ys   = Poly (xs `mul` ys)

{- To be implemented
data PolyFrac = Poly :%% Poly

instance Num PolyFrac where
    negate (p %% q)       = (negate p %% q)
    signum (Poly [] %% _) = Poly []
    signum (_ %% Poly []) = undefined
    signum (p %% q)       = signum p / signum q
    abs (p %% q)          = (abs p) %% (abs q)
    (p %% q) + (p' %% q') = simplify ((p * q' + q * p') %% (q * q'))
    (p %% q) * (p' %% q') = simplify ((p * p') %% (q * q'))
    px - py               = px + (negate py)
    fromInteger integer   = fromInteger integer %% fromInteger 1

instance Fractional PolyFrac where
-}
    
add, mul :: Num a => [a] -> [a] -> [a]
add xs [] = xs
add [] ys = ys
add (x:xs) (y:ys) = (x + y) : add xs ys
mul xs [] = []
mul [] ys = []
mul (x:xs) ys = (map (x *) ys) `add` (0 : mul xs ys)

div' :: Rats -> Rats -> Rats -> (Rats, Rats)
div' xs@(x:_) ys@(y:_) q = 
    if length ys > length xs
    then (q, dropWhile (==0) xs)
    else let t = - x / y in div' (tail $ xs `add` (map (t *) ys)) ys (- t : q)

pairFromRational :: Rational -> (Integer, Integer)
pairFromRational x = (numerator x, denominator x)

fromDescList, fdl :: Rats -> Poly
fromDescList xs = Poly (reverse xs)
fdl = fromDescList -- short hand for GHCi

degree :: Poly -> Int
degree (Poly xs) = length xs - 1

showAsc, showDesc :: Poly -> String
showAsc  (Poly xs) = pprint xs True
showDesc (Poly xs) = pprint xs False

doubleton :: Rational -> Poly
doubleton x = let (n, d) = pairFromRational x in Poly (map toRational [-n, d])

(#>) :: Rational -> Poly -> Rational
put #> (Poly xs) = sum $ zipWith (*) xs (iterate (* put) 1)

(</>) :: Poly -> Poly -> DivResult
_ </> (Poly []) = DivByZero
(Poly xs) </> (Poly ys) = case div' (reverse xs) (reverse ys) [] of
    (q, []) -> Quotient (Poly q)
    (q, r)  -> QuotRem  (Poly q) (fromDescList r)

divides :: Integer -> Integer -> Bool
a `divides` b = mod a b == 0

factors :: Poly -> Rats
factors (Poly [ ]) = []
factors (Poly [x]) = []
factors p@(Poly (1:xs)) = [t | n <- ls nr, d <- ls dr, s <- [1, -1], let t = s * (d % n), t #> p == 0]
    where (nr, dr) = pairFromRational (last xs)
          ls x = [a | a <- [1..x], x `divides` a]
factors otherwise = factors (abs otherwise)

-- data PolyFrac = Poly %% Poly
