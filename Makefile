CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell

src/PHP/Simple/SimpleAst.hs : src/PHP/Simple/SimpleAst.ag 
	uuagc -dcfws -P src/PHP/Simple src/PHP/Simple/SimpleAst.ag

haskell : src/PHP/Simple/SimpleAst.hs
	cabal install

.PHONY : haskell
