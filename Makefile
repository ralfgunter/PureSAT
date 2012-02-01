all: dpll.hs
	@ghc -O3 -o sat dpll.hs

clean:
	@rm sat dpll.hi dpll.o
