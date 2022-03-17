import System.Exit

import Lib
import Order
import Vwap

main :: IO ()
main = do
    -- Test 1: Lib.splitByChar
    let test1_input = "a\nb\nc"
    let test1_output = splitByChar (=='\n') test1_input
    let test1_expectedOutput = ["a","b","c"]
    if test1_output == test1_expectedOutput 
        then do
            putStr "\nsplitByChar - test 1 success"
        else do 
            putStrLn "\nsplitByChar - test 1 failed!"
            exitWith (ExitFailure 1)

    -- Test 2: Order.stringToOrder
    let test2_input = "Bluebox BB777,Yew 294,BUSU1,Bid,44,25"
    let test2_output = stringToOrder test2_input
    let test2_expected_symbol = "BUSU1"
    let test2_expected_price = 44
    let test2_expected_quantity = 25
    if (getProductSymbol test2_output) == test2_expected_symbol
        then do
            if (getPrice test2_output) == test2_expected_price
                then do
                    if (getQuantity test2_output) == test2_expected_quantity
                        then do
                            putStr "\nstringToOrder - test 2 success"
                        else do
                            putStrLn "\nstringToOrder - test 2 failed!"
                            exitWith (ExitFailure 1)
                else do
                    putStrLn "\nstringToOrder - test 2 failed!"
                    exitWith (ExitFailure 1)
        else do 
            putStrLn "\nstringToOrder - test 2 failed!"
            exitWith (ExitFailure 1)

    -- Test 3: Vwap.createVwap
    let test3_input_pq = 51
    let test3_input_q = 17
    let test3_output = createVwap test3_input_pq test3_input_q
    let test3_expected_pq = 51
    let test3_expected_q = 17
    if (getSumPq test3_output) == test3_expected_pq
        then do
            if (getSumQ test3_output) == test3_expected_q
                then do
                    putStrLn "\ncreateVwap - test 3 success"
                else do
                    putStrLn "\ncreateVwap - test 3 failed!"
                    exitWith (ExitFailure 1)
        else do 
            putStrLn "\ncreateVwap - test 3 failed!"
            exitWith (ExitFailure 1)

