#ifndef NQ
#define NQ

//#include <cryptominisat4/cryptominisat.h>
#include "glucose-syrup/core/Solver.h"
#include <vector>
#include <string>
#include <iostream>
#include <cassert>

using std::vector;
using namespace Glucose;

typedef vec<Lit> Lits;

inline
ulong phantom_int(ulong i, ulong j, ulong n) {
  return (((i)*(n) + (j))*2 + 1);
    }

inline
Lit phantom(ulong i, ulong j, ulong n, bool truth) {
  return mkLit(phantom_int(i,j,n), !truth);
}

inline
ulong queen_int(ulong i, ulong j, ulong n) {
  return (i*n + j)*2;
}

inline
Lit queen(ulong i, ulong j, ulong n, bool truth) {
  return mkLit(queen_int(i,j,n), !truth);
}

void add_col(ulong i, ulong j, ulong n, Lit phant, Solver& s);
void add_diagr(ulong i, ulong j, ulong n, Lit phant, Solver& s);
void add_diagl(ulong i, ulong j, ulong n, Lit phant, Solver& s);
void print_solution(const vec<lbool>& m, ulong n);
void check_solution(const vec<lbool>& m, ulong n);
#endif
