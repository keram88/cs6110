nqueens: nqueens.cc nqueens_tools.o minisat
	g++ -std=c++11 -Wall -Wno-literal-suffix -Wno-parentheses nqueens.cc \
	nqueens_tools.o -o nqueens -I`pwd`/minisat minisat/core/Solver.or


nqueens_tools.o: nqueens_tools.cc nqueens_tools.hh minisat
	g++ -c -Wall -Wno-sign-compare -Wno-parentheses nqueens_tools.cc -I`pwd`/minisat

minisat: 
	./setup.sh

clean:
	@rm -fr nqueens nqueens_tools.o

reallyclean:
	@rm -fr minisat minisat-2.2.0.tar.gz
