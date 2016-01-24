nqueens: nqueens.cc nqueens_tools.o cryptominisat-4.5.3
	clang++ -O3 -Wall -march=native -std=c++11 nqueens.cc -o nqueens nqueens_tools.o -Icryptominisat-4.5.3/build/include \
	-Lcryptominisat-4.5.3/build/lib -lcryptominisat4 -lpthread

nqueens_tools.o: nqueens_tools.cc nqueens_tools.hh cryptominisat-4.5.3
	clang++ -O3 -Wall -march=native -std=c++11 -c nqueens_tools.cc -o nqueens_tools.o -Icryptominisat-4.5.3/build/include

cryptominisat-4.5.3: setup.sh
	./setup.sh

clean:
	@rm -fr nqueens nqueens_tools.o

reallyclean:
	@rm -fr cryptominisat-4.5.3 4.5.3.tar.gz

