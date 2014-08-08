module Binary (
      toAscii
    , toBits
    ) where

import Data.Char

type Bit = Int

-- Convert 8 bits to 0-255 char
toAscii :: [Bit] -> Char
toAscii bits = chr $ sum $ zipWith (*) bits $ map (2^) [7, 6..0]

-- Convert 0-255 char to 8 bit list
toBits :: Char -> [Bit]
toBits c = reverse $ snd $ foldr f (ord c, []) $ map (2^) [0..7]
    where
        f b (rem, l)
            | rem >= b = (rem - b, 1 : l)
            | otherwise = (rem, 0 : l)
