.PHONY: all build exec clean

all: build

build:
	@echo 'Building...'
	@stack build

exec:
	@stack exec haskell-json-parser-exe < sample-data.csv

run-tests:
	@stack test

clean:
	@echo 'Cleaning current directory...'
	@rm -rf .stack-work/