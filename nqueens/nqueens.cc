// Mark S. Baranowski - u0485301 - CS 6100 - HW1
#include <cassert>
#include <iostream>
#include <string>
#include "nqueens_tools.hh"
#include "tune.h"

int main(const int argc, const char** args)
{
  ulong n = 0;
  long solutions = 1;

  switch(argc) {
  case 1:
    std::cout << "Please give a number" <<std::endl;
    return 0;
    break;
  case 3: {
    // A negative third argument means all solutions
    std::string solc_s(args[2]);
    try {
      solutions = stol(solc_s);
      if(solutions == 0) {
	std::cout << "Okay. No solutions." << std::endl;
	return 0;
      }
    }
    catch(...) {
      std::cout << "Could not convert solutions to a number" << std::endl;
      return 0;
    }
  }
  case 2: {
    long arg_n = 0;
    std::string argn_s(args[1]);
    try {
      arg_n = stoul(argn_s);
      if(!(arg_n > 0)) {
	std::cout << "Enter a positive number" << std::endl;
	return 0;
      }
      n = arg_n;
    }
    catch(...) {
      std::cout << "Could not convert n to a number" << std::endl;
      return 0;
    }
    break;
  }
  default:
    std::cout << "Too many args" << std::endl;
    return 0;
    break;
  }

  bool polarity = false;
  if(n >= 1 and n <= 251)
      polarity = tune_param[n];
  
  Solver solver;
  Lits clause;

  // We need 2*n*n variables
  for(ulong i = 0; i < 2*n*n; ++i)
    solver.newVar(polarity);

  // Generate the constraints
  for(ulong i = 0; i < n; ++i) {
    Lits phantoms(n);
    for(ulong j = 0; j < n; ++j) {
      Lits row(2);
      row[0] = phantom(i, j, n, false);
      row[1] = queen(i, j, n, true);
      solver.addClause_(row);
      add_col  (i, j, n, phantom(i,j,n,false), solver);
      add_diagr(i, j, n, phantom(i,j,n,false), solver);
      add_diagl(i, j, n, phantom(i,j,n,false), solver);
      phantoms[j] = phantom(i,j,n,true);
    }
    solver.addClause_(phantoms);
  }
  solver.simplify();
  long found = 0;
  while((found < solutions || solutions < 0) && solver.solve()) {
    found++;
    const auto& model = solver.model;
    
    print_solution(model, n);
    check_solution(model, n);
    
    Lits new_cons;
    for(ulong i = 0; i < n; ++i)
      for(ulong j = 0; j < n; ++j) {
	new_cons.push(queen(i,j,n, !(model[queen_int(i,j,n)] == l_True)));
      }
    solver.addClause_(new_cons);
  };
  
  if(found == 0)
    std::cout << "No solution" << std::endl;
  else if(solutions != 1)
    std::cout << found << std::endl;
  return 0;
}
