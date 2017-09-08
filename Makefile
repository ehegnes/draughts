
.PHONY: all install test doc

all: test install
	stack build --pedantic --copy-bins

install:
	stack build --copy-bins

test:
	# Code Coverage
	stack test --coverage
	
	# Cyclomatic Complexity
	argon src
	
	# Static Analysis
	hlint src
	
	# Documentation Coverage
	stack haddock

