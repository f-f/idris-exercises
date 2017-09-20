module Main

import Data.Vect

-- Tree Stuff

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right)
       = case compare x val of
              LT => Node (insert x left) val right
              EQ => orig
              GT => Node left val (insert x right)

-- Ex.1 listToTree
listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)


-- Ex.2 treeToList
treeToList : Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ val :: (treeToList right)

-- treeToList $ listToTree [4,1,8,7,2,3,9,5,6]
-- => [1, 2, 3, 4, 5, 6, 7, 8, 9] : List Integer


-- Ex.3 datatype for arithmetic expressions
data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

-- Ex.4 evaluating expressions
evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = (evaluate x) + (evaluate y)
evaluate (Sub x y) = (evaluate x) - (evaluate y)
evaluate (Mult x y) = (evaluate x) * (evaluate y)

-- evaluate (Mult (Val 10) (Add (Val 6) (Val 3)))
-- => 90 : Int


-- Ex.5 maxMaybe, returns the larger of the two inputs,
-- or Nothing if both inputs are Nothing
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing x = x
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y) = Just $ max x y

-- Ex.6 biggestTriangle, return the area of the biggest triangle in a picture, or Nothing
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic1 pic2) = pictureArea pic1 + pictureArea pic2
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive t@(Triangle x y)) = Just $ area t
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Rotate x p) = biggestTriangle p
biggestTriangle (Translate x y p) = biggestTriangle p
biggestTriangle (Combine p1 p2) = maxMaybe (biggestTriangle p1)
                                           (biggestTriangle p2)

-- biggestTriangle testPic1
-- => Just 4.0 : Maybe Double
-- biggestTriangle testPic2
-- => Nothing : Maybe Double


-- Dependent types

-- Ex.3,4 vectTake, take from a vector

vectTake : (i : Fin n) -> Vect n a -> Vect (finToNat i) a
vectTake FZ xs = []
vectTake (FS k) (x :: xs) = x :: vectTake k xs

-- Ex.5 sumentries: given two lists, sum the elements at an index, if index in vect
sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                (Just i) => Just $ (index i xs) + (index i ys)

-- sumEntries 2 [1,2,3,4] [5,6,7,8]
-- => Just 10 : Maybe Integer

