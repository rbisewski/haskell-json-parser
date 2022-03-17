module Main where

import Lib
import Order
import Vwap

import Control.Monad
import Data.Int
import qualified Data.HashTable.IO as H
import Numeric.Natural

type HashTable k v = H.CuckooHashTable k v

main :: IO ()
main = do
    -- stdin
    contents <- getContents

    -- define a map to store VWAPs
    vwapMap <- H.new :: IO (HashTable String Vwap)

    -- loop thru each line
    let lines = splitByChar (=='\n') contents
    forM_ lines $ \i -> do

        -- get the symbol/price/quantity from the given line
        let order = stringToOrder i
        let productSymbol = getProductSymbol order
        let price = getPrice order
        let quantity = getQuantity order
        let quantityAsInt64 = getQuantityAsInt64 order

        -- do a quick lookup to determine if the symbol maybe already exists
        maybeElement <- H.lookup vwapMap productSymbol
        let element = maybeToVwap maybeElement

        -- if it does, use that value and add the new sum to it
        let sumPq = (getSumPq element) + (price * quantityAsInt64)
        let sumQ = (getSumQ element) + quantity

        -- re-insert the new value
        let newVwap = createVwap sumPq sumQ
        H.insert vwapMap productSymbol newVwap

    -- create the JSON output and put it to stdout
    putStrLn "{"

    list <- H.toList(vwapMap)
    forM_ list $ \(k,v) -> do

      -- figure out where we are on the list
      H.delete vwapMap k
      remaining <- H.toList(vwapMap)
      let l = length remaining

      -- print out the json
      let sumPq = getSumPqAsDouble v
      let sumQ = getSumQAsDouble v
      let vwapValue = sumPq / sumQ
      let vwapString = show (vwapValue)
      let volume = show (getSumQ v)
      let json = "\"" ++ k ++ "\": {\"vwap\": " ++ vwapString ++ ", \"volume\": " ++ volume ++ "}"
      putStr json

      -- use the length to decide whether this JSON element is the last
      if l == 0
        then putStrLn ""
        else putStrLn ","

    putStrLn "}"
