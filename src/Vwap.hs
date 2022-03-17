--
-- Vwap struct and methods
--
module Vwap
    ( createVwap,
      maybeToVwap,
      getSumPq,
      getSumPqAsDouble,
      getSumQ,
      getSumQAsDouble,
      Vwap
    ) where

import Data.Int
import Numeric.Natural

data Vwap = Vwap { 
  sumOfPriceAndQuantity :: Int64,
  sumOfQuantity :: Natural
} deriving (Show)

createVwap :: Int64 -> Natural -> Vwap
createVwap a b = Vwap {sumOfPriceAndQuantity=a, sumOfQuantity=b}

maybeToVwap :: (Maybe Vwap) -> Vwap
maybeToVwap m = case m of
                    Just n -> n
                    Nothing -> Vwap { sumOfPriceAndQuantity=0, sumOfQuantity=0 }

getSumPq :: (Vwap) -> Int64
getSumPq s = sumOfPriceAndQuantity s

getSumPqAsDouble :: (Vwap) -> Double
getSumPqAsDouble s = do
                    let casted = show (sumOfPriceAndQuantity s)
                    read casted :: Double

getSumQ :: (Vwap) -> Natural
getSumQ s = sumOfQuantity s

getSumQAsDouble :: (Vwap) -> Double
getSumQAsDouble s = do
                    let casted = show (sumOfQuantity s)
                    read casted :: Double
