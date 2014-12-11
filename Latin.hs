{--
Latin.hs
--}

import           Control.Monad (replicateM)
import           Data.List     (nub, permutations, transpose)

latinSquares :: Int -> [[[Int]]]
latinSquares n = filter checkRows .
                 map transpose .
                 replicateM n .
                 permutations $ [1..n]
    where checkRows = all (\xs -> length xs == length (nub xs))


