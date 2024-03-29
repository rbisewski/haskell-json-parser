# Haskell JSON parser

This readme file contains documentation about the Haskell JSON parser to calculate the vwap of some given orders.

Created using "stack new", so some of the files have been autogenerated.

A brief summary of the important files:

```
| 
\_app
|    \_Main.hs                # Main application
|
\_src
|    \_Order.hs               # Order structure and related functions
|    \_Vwap.hs                # Vwap structure and related functions
|    \_Lib.hs                 # Misc functions
|
|
\_test
|     \_Spec.hs               # Small test suite written to test major functions
|
+--Makefile                   # File for make targets
+--sample-data.csv            # Example data for testing with
+--package.yaml               # Stack package file
|
|
+--stack.yaml                 # Autogenerated
+--haskell-json-parser.yaml   # Autogenerated
```

## Summary of design

The overall design of the program can be summarized as:

* was created using the latest haskell + stack + ghc, on Linux
    * can compile it via the Makefile target `make build`

* reads in CSV data of the form shown in `sample-data.csv`, and you can run it using either of these two commands:
    * `stack exec haskell-json-parser-exe < sample-data.csv`
    * `make exec`

* relies on a hashtable
    * input inserted easily with `H.insert`
    * output can be retrieved easily via `H.lookup`

* takerSide is its own enum type called `Side` that can either `Ask` or `Bid`, as the PDF denoted it as a defined type

* price is stored as an `Int64` as the PDF mentioned `int64`

* quantity is stored as a `Natural` since the PDF mentioned `uint32`

* two main data structures
    * `Order` for storing each line of the CSV
    * `Vwap` for storing Sum(pq) and Sum(q)
    * implemented it this way for future expansion of those modules, if needed

* contains a number of tests of the major functions that can be using either of the two commands below
    * `stack test`
    * `make run-tests`

* prints volume weighted average price, per symbol, to JSON on stdout

Finally, to make this more straightforward to use, I've included a Makefile with some targets.

## Building

To build this project:

```bash
make build
```

## Run with provide sample data

To run this project with the example file provided `sample-data.csv` the following Makefile target has been provided for your convenience:

```bash
make exec
```

If it compiles and runs correctly, should get the following output:

```
{
"BUSU1": {"vwap": 43.42857142857143, "volume": 35},
"BUSM2": {"vwap": 43250.0, "volume": 23},
"BUIZ1": {"vwap": -2.0, "volume": 14}
}
```

## Tests

To run the tests:

```bash
make run-tests
```

If it all compiles and then runs correctly, should get something like:

```
haskell-json-parser> test (suite: haskell-json-parser-test)
            
Progress 1/2: haskell-json-parser
splitByChar - test 1 success
stringToOrder - test 2 success
createVwap - test 3 success
                                  
haskell-json-parser> Test suite haskell-json-parser-test passed
```
