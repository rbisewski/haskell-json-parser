--
-- Order struct definitions
--
module Order
    ( stringToOrder,
      getProductSymbol,
      getPrice,
      getQuantity,
      getQuantityAsInt64,
      Order
    ) where

import Lib
import Data.Int
import Numeric.Natural

data Side = Bid | Ask deriving (Enum)

data Order = Order {
  makerAccountId :: String,
  takerAccountId :: String,
  productSymbol :: String,
  takerSide :: Side,
  price :: Int64,
  quantity :: Natural
}

stringToOrder :: String -> Order
stringToOrder o = do
                    let orderComponents = splitByChar (==',') o
                    let makerAccountId = orderComponents !! 0
                    let takerAccountId = orderComponents !! 1
                    let productSymbol = orderComponents !! 2

                    let takerSideString = orderComponents !! 3
                    let takerSide = case takerSideString of
                                      "Bid" -> Bid
                                      "Ask" -> Ask

                    let priceString = orderComponents !! 4
                    let quantityString = orderComponents !! 5

                    let price = read priceString :: Int64
                    let quantity = read quantityString :: Natural

                    Order {
                      makerAccountId=makerAccountId,
                      takerAccountId=takerAccountId,
                      productSymbol=productSymbol,
                      takerSide=takerSide,
                      price=price,
                      quantity=quantity
                    }

getProductSymbol :: Order -> String
getProductSymbol o = productSymbol o

getPrice :: Order -> Int64
getPrice o = price o

getQuantity :: Order -> Natural
getQuantity o = quantity o

getQuantityAsInt64 :: Order -> Int64
getQuantityAsInt64 o = do
                  let quantityString = show (quantity o)
                  read quantityString :: Int64