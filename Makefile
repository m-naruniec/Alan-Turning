all:
	mkdir build
	cd build;\
	cp ../src/* . ;\
	happy -gca ParTurning.y;\
	alex -g LexTurning.x;\
	ghc --make interpreter.hs -o interpreter

clean:
	cd build;\
	rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	cd build;\
	rm -f DocTurning.* LexTurning.* ParTurning.* LayoutTurning.* SkelTurning.* PrintTurning.* TestTurning.* AbsTurning.* SemTurning.* TypeTurning.hs interpreter.hs interpreter TestTurning ErrM.* SharedString.* ComposOp.* turning.dtd XMLTurning.* Makefile*

