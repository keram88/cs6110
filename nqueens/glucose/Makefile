nqueens: nqueens.cc nqueens_tools.o glucose-syrup
	clang++ -std=c++11 -Wall -Wno-parentheses nqueens.cc nqueens_tools.o -o nqueens \
	-I`pwd`/glucose-syrup glucose-syrup/utils/System.or glucose-syrup/utils/Options.or \
	glucose-syrup/core/Solver.or


nqueens_tools.o: nqueens_tools.cc nqueens_tools.hh glucose-syrup
	clang++ -c -Wall -Wno-parentheses nqueens_tools.cc -I`pwd`/glucose-syrup

glucose-syrup:
	./setup.sh

clean:
	@rm -fr nqueens nqueens_tools.o

reallyclean:
	@rm -fr glucose-syrup glucose-syrup.tgz

