import Cellular
import Graphics.Pgm
import Data.Array
import Data.Word
import System.Environment( getArgs )
import System.Console.GetOpt

main = do
    args <- getArgs
    let [size, code', bound] = fmap read args :: [Int]
        code = fromIntegral code' :: Word8
    u' <- randomUniverse bound
    let u = if bound == 0 then lonelyUniverse else u'
    arrayToFile "rule90.pgm" $ fmap boolToWord16 $ toArray size (rule code) u

toArray n rule u = listArray ((0,0), (height, width)) list
  where
    list = concat grid
    grid = toGrid n rule u
    width = length (grid!!0) - 1
    height = length grid - 1

boolToWord16 :: Bool -> Word16
boolToWord16 False = -1
boolToWord16 True  = 0
