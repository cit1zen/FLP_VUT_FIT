all:
	ghc main.hs -o plg-2-nka

.PHONY: test
test: 
	shell
	./plg-2-nka -1 tests/test-1/test.in
	diff tests/test-1/test.out

.PHONY: clean
clean:
	rm ./plg-2-nka ./*.o ./*.hi
