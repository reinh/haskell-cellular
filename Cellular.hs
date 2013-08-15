module Cellular where

import Control.Comonad
import Control.Monad
import System.Random
import Control.Lens
import Data.Bits
import Data.Bits.Lens
import Data.Word

-- U universe zipper
data U a = U [a] a [a] deriving (Show, Eq)

left, right :: U a -> U a
left (U (l:ls) x rs) = U ls l (x:rs)
right (U ls x (r:rs)) = U (x:ls) r rs

instance Functor U where
    fmap f (U ls x rs) = U (fmap f ls) (f x) (fmap f rs)

instance Comonad U where
    extract (U _ x _) = x
    duplicate a = U (tail $ iterate left a) a (tail $ iterate right a)

toChar :: Bool -> Char
toChar True  = '#'
toChar False = '.'

toList :: Int -> U a -> [a]
toList n (U ls x rs) = reverse (take n ls) ++ [x] ++ take n rs

alive :: [Bool] -> Int
alive = length . filter id

neighborsU :: U a -> [a]
neighborsU (U (l:ls) x (r:rs)) = [l,r]

lonelyUniverse, emptyUniverse :: U Bool
lonelyUniverse = U (repeat False) True (repeat False)
emptyUniverse = U (repeat False) False (repeat False)

randomUniverse :: Int -> IO (U Bool)
randomUniverse ubound = do
    g <- newStdGen
    let rands    = randomRs (1,ubound)
        (x, g')  = randomR (1,ubound) g
        (gl, gr) = split g'
    return $ fmap (==1) $ U (rands gl) x (rands gr)

rule :: Word8 -> U Bool -> Bool
rule w (U (l:_) m (r:_)) = testBit w (0 & partsOf bits .~ [r,m,l])

rule90 :: U Bool -> Bool
rule90 (U (l:_) _ (r:_)) = l `xor` r
  where x `xor` y = (x || y) && (not (x && y))

rule90' (U (l:_) _ (r:_)) = l `xor` r
  where
    True  `xor` True  = False
    True  `xor` False = True
    False `xor` True  = True
    False `xor` False = False

rule90'' u = count `mod` 2 == 1
  where count = (alive.neighborsU) u

evolve :: Comonad w => (w a -> a) -> w a -> [w a]
evolve = iterate . extend

showU :: Int -> U Bool -> String
showU n u = fmap toChar $ toList n u

generateU :: Comonad w => Int -> (w a -> a) -> w a -> [w a]
generateU n rule u = take n $ evolve rule u

toGrid n rule = fmap (toList n) . generateU n rule

timeSpaceDiagram rule n u = mapM_ (putStrLn . (fmap toChar)) $ toGrid n rule u

triangle :: Int -> U Bool -> IO ()
triangle = timeSpaceDiagram rule90
