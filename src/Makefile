all:
	happy -gca ParTurning.y
	alex -g LexTurning.x
	ghc --make interpreter.hs -o interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocTurning.* LexTurning.* ParTurning.* LayoutTurning.* SkelTurning.* PrintTurning.* TestTurning.* AbsTurning.* TestTurning ErrM.* SharedString.* ComposOp.* turning.dtd XMLTurning.* Makefile*

