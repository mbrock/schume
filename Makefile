
CFLAGS = -Wall -W -g -std=gnu99

all: schumec vm/vm

schumec: Main.hs Schume/Compiler.hs Syntax/Abs.hs Schume/Pretty.hs \
         Schume/Bytecode.hs Schume/Codegen.hs
	ghc --make Syntax/Abs.hs Syntax/Par.hs Syntax/Lex.hs
	ghc --make -Wall Main.hs -o schumec

clean:
	rm -f *.o *.hi Schume/*.o Schume/*.hi schumec
	rm -f vm/ops.h
	rm -rf Syntax
	rm -rf TeX

vm/ops.h: Schume/Bytecode.hs Schume/BytecodeHeaderGenerator.hs
	mkdir -p vm
	runghc Schume/BytecodeHeaderGenerator.hs > vm/ops.h

Syntax/Abs.hs: Syntax.cf
	bnfc -d Syntax.cf
	alex -g Syntax/Lex.x
	happy -gca Syntax/Par.y

# TeX/Compiler.pdf: Document.lhs Schume/Compiler.lhs
# 	mkdir -p TeX
# 	lhs2TeX -o TeX/Compiler.tex Document.lhs
# 	( cd TeX ; pdflatex Compiler.tex ; pdflatex Compiler.tex ; cd .. )

# TeX/Compiler.ps: TeX/Compiler.pdf
# 	( cd TeX ; latex Compiler.tex ; latex Compiler.tex ; \
#           dvips Compiler ; cd .. )
