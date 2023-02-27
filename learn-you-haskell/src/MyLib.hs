module MyLib where
--module MyLib (Tree,dtree) where

data Tree a = Leaf { element :: a }
            | Fork { element :: a
                   , left :: Tree a
                   , right :: Tree a
                   } deriving Show

dtree :: Tree Integer
dtree = dtree' 0 where
    dtree' depth = Fork { element = depth
                        , left = dtree' (depth + 1)
                        , right = dtree' (depth + 1)
                        }

mean' :: [Double] -> Double
mean' xs = let (res, len ) = foldl (\(m, n) x -> (m + x / len, n + 1)) (0, 0) xs in res

-- Chapter 5

class BoolLike a where
    fromBoolLike :: a -> Bool

instance BoolLike Int where
    fromBoolLike = (0 /=)

instance BoolLike (Maybe a) where
    fromBoolLike Nothing = False
    fromBoolLike (Just _) = True

instance BoolLike Bool where
    fromBoolLike x = x

data A = A
data B = B

class ToInt a where
    toInt :: a -> Int

instance ToInt A where
    toInt _ = 1

instance ToInt B where
    toInt _ = 2

add :: ToInt x => x -> x -> Int
add x y = toInt x + toInt y

add' :: (ToInt x, ToInt y) => x -> y -> Int
add' x y = toInt x + toInt y

square :: Integer -> Maybe Integer
square n
    | 0 <= n = Just (n * n)
    | otherwise = Nothing

squareRoot :: Integer -> Maybe Integer
squareRoot n
    | 0 <= n = squareRoot' 1
    | otherwise = Nothing
    where
        squareRoot' x
            | n > x * x = squareRoot' (x + 1)
            | n < x * x = Nothing
            | otherwise = Just x

{--
squareAndSquareRoot2 :: Integer -> Integer -> Maybe Integer
squareAndSquareRoot2 m n = case square m of
                             Nothing -> Nothing
                             Just mm -> case square n of
                                          Nothing -> Nothing
                                          Just nn -> squareRoot (mm * nn)
 --}

squareAndSquareRoot1 :: Integer -> Maybe Integer
squareAndSquareRoot1 n = do
  nn <- square n
  squareRoot nn

squareAndSquareRoot2 :: Integer -> Integer -> Maybe Integer
squareAndSquareRoot2 m n = do
  mm <- square m
  nn <- square n
  squareRoot (mm * nn)

lessThan :: Integer -> [Integer]
lessThan n = [0 .. n-1]

plusMinus :: Integer -> Integer -> [Integer]
plusMinus a b = [a + b, a - b]

{--
allPM0s :: Integer -> [Integer]
allPM0s n = concat (map (plusMinus 0) (lessThan n))

allPMs :: Integer -> Integer -> [Integer]
allPMs m n = concat (map (\x -> concat (map (plusMinus x) (lessThan n))) (lessThan m))
--}

allPM0s :: Integer -> [Integer]
allPM0s n = do
  x <- lessThan n
  plusMinus 0 x

allPMs :: Integer -> Integer -> [Integer]
allPMs m n = do
  x <- lessThan m
  y <- lessThan n
  plusMinus x y

-- countOdd :: ((->) [Int]) Int
countOdd :: [Int] -> Int
countOdd = length . filter odd

countEven :: [Int] -> Int
countEven = length . filter even

{--
countAll :: [Int] -> Int
countAll xs = countOdd xs + countEven xs
--}

countAll :: [Int] -> Int
countAll = do
  odds  <- countOdd
  evens <- countEven
  return (odds + evens)

{--
countAll :: [Int] -> Int
countAll = countOdd >>= (\odds -> countEven >>= (\evens -> return (odds + evens)))
--}
