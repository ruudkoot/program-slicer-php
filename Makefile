CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell

src/PHP/Simple/SimpleAst.hs : src/PHP/Simple/SimpleAst.ag 
	uuagc -dcfws -P src/PHP/Simple src/PHP/Simple/SimpleAst.ag

haskell : src/PHP/Simple/SimpleAst.hs
	runhaskell Setup.lhs configure $(CABAL-CONFIGURE-FLAGS)
	runhaskell Setup.lhs build $(CABAL-BUILD-FLAGS)

.PHONY : haskell
