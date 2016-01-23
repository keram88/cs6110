#include <cryptominisat4/cryptominisat.h>
#include <cassert>
#include <vector>
#include <iostream>
#include <string>

using std::vector;
using namespace CMSat;

typedef vector<Lit> Lits;

inline
ulong phantom_int(ulong i, ulong j, ulong n) {
  return (i*n + j)*2 + 1;
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

void add_col(ulong i, ulong j, ulong n, Lit phant, SATSolver& s) {
  Lits result(2);
  for(ulong k = 0; k < n; ++k) {
    if(k == i)
      continue;
    result[0] = phant;
    result[1] = queen(k, j, n, false);
    s.add_clause(result);
  }
}

void add_diagr(ulong i, ulong j, ulong n, Lit phant, SATSolver& s) {
  Lits result(2);
  for(ulong k = 1; k < std::min(n-i,n-j); ++k) {
    result[0] = phant;
    result[1] = queen(i+k, j+k, n, false);
    s.add_clause(result);
  }
}

void add_diagl(ulong i, ulong j, ulong n, Lit phant, SATSolver& s) {
  Lits result(2);
  for(ulong k = 1; k < std::min(j+1,n-i); ++k) {
    result[0] = phant;
    result[1] = queen(i+k, j-k, n, false);
    s.add_clause(result);
  }
}

void print_solution(const vector<lbool>& m, ulong n) {
  for(ulong i = 0; i < n; ++i) {
    for(ulong j = 0; j < n; ++j) {
      std::cout << (m[queen_int(i,j,n)] == l_True ? 'X' : '.');
    }
    std::cout << "\n";
  }
  std::cout << std::endl;
}

int main(const int argc, const char** args)
{
  ulong n = 0;

  switch(argc) {
  case 1:
    std::cout << "Please give a number" <<std::endl;
    return 0;
    break;
  case 2: {
    std::string arg(args[1]);
    try {
      n = stoul(arg);
      if(!(n > 0)) {
	std::cout << "Enter a positive number" << std::endl;
	return 0;
      }
    }
    catch(...) {
      std::cout << "Could not convert arg to a number" << std::endl;
      return 0;
    }
    break;
  }
  default:
    std::cout << "Too many args";
    return 0;
    break;
  }
  
  SATSolver solver;
  vector<Lit> clause;
  solver.set_no_simplify();
  //Let's use 4 threads
  solver.set_num_threads(4);

  // We need 2*n*n variables
  solver.new_vars(2*n*n);

  // Generate the constraints
  for(ulong i = 0; i < n; ++i) {
    Lits phantoms(n);
    for(ulong j = 0; j < n; ++j) {
      solver.add_clause(Lits{phantom(i, j, n, false),
                  	     queen(i, j, n, true)});
      add_col  (i, j, n, phantom(i,j,n,false), solver);
      add_diagr(i, j, n, phantom(i,j,n,false), solver);
      add_diagl(i, j, n, phantom(i,j,n,false), solver);
      phantoms[j] = phantom(i,j,n,true);
    }
    solver.add_clause(phantoms);
  }

  lbool ret = solver.solve();
  if(ret == l_True) {
    const auto& model = solver.get_model();
    print_solution(model, n);
  }
  else {
    std::cout << "No solution" << std::endl;
  }

  return 0;
}
