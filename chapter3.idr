module Main

import Data.Vect

length' : List a -> Nat
length' [] = 0
length' (x :: xs) = 1 + length' xs


reverse' : List a -> List a
reverse' [] = []
reverse' l = rev l []
  where
    rev : List a -> List a -> List a
    rev []        ys = ys
    rev (x :: xs) ys = rev xs (x::ys)

reverseHelper : Vect n a -> Vect m a -> Vect (n + m) a
reverseHelper [] ys = ys
reverseHelper {n=S len} {m} (x :: xs) ys =
  rewrite (plusSuccRightSucc len m)
  in reverseHelper xs (x :: ys)

reverse'' : Vect n a -> Vect n a
reverse''     [] = []
reverse'' {n} l  =
 rewrite sym (plusZeroRightNeutral n)
 in reverseHelper l []

map' : (a -> b) -> List a -> List b
map' f [] = []
map' f (x :: xs) = f x :: map' f xs

map'' : (a -> b) -> Vect n a -> Vect n b
map'' f [] = []
map'' f (x :: xs) = f x :: map'' f xs


-- Matrix stuff

-- Ex.1 implement transposeMat with zipWith
createEmpties : Vect n (Vect 0 elem)
createEmpties {n} = replicate n []

transposeMatrix : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMatrix [] = createEmpties
transposeMatrix (x :: xs) = let xsTrans = transposeMatrix xs in
                            zipWith (::) x xsTrans

-- Ex.2 addMatrix
addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix xs ys = zipWith (zipWith (+)) xs ys

-- Ex.3 multiplying matrices
multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix nm mp = map (\m => map (\p => sum $ zipWith (*) m p) pm) nm
                   where
                     pm = transposeMatrix mp
