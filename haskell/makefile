all : recursive_parser simple_parser 

simple_parser : datatypepaser.hs
	ghc -package parsec -o $@ $^

recursive_parser : recursiveparser.hs
	ghc -package parsec -o $@ $^
