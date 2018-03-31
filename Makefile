all:
	ghc main.hs -o plg-2-nka

.PHONY: clean
clean:
	rm ./plg-2-nka ./*.o
