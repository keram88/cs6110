#ifndef NQ
#define NQ

#include <cryptominisat4/cryptominisat.h>
#include <vector>
#include <string>
#include <cassert>

using std::vector;
using namespace CMSat;
using std::vector;
using namespace CMSat;

typedef vector<Lit> Lits;

inline
ulong phantom_int(ulong i, ulong j, ulong n) {
  return (((i)*(n) + (j))*2 + 1);
    }

inline
Lit phantom(ulong i, ulong j, ulong n, bool truth) {
  return Lit(phantom_int(i,j,n), !truth);
}

inline
ulong queen_int(ulong i, ulong j, ulong n) {
  return (i*n + j)*2;
}

inline
Lit queen(ulong i, ulong j, ulong n, bool truth) {
  return Lit(queen_int(i,j,n), !truth);
}

void add_col(ulong i, ulong j, ulong n, Lit phant, SATSolver& s);
void add_diagr(ulong i, ulong j, ulong n, Lit phant, SATSolver& s);
void add_diagl(ulong i, ulong j, ulong n, Lit phant, SATSolver& s);
void print_solution(const vector<lbool>& m, ulong n);
void check_solution(const vector<lbool>& m, ulong n);
#endif
