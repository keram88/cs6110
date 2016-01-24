#include "nqueens_tools.hh"

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

void check_solution(const vector<lbool>& m, ulong n) {
  ulong queen_count = 0;
  for(ulong r = 0; r < n; ++r) {
    for(ulong c = 0; c < n; ++c) {
      if(m[queen_int(r,c,n)] == l_False) continue;
      queen_count++;
      // Check column
      for(ulong r_c = 0; r_c < n; ++r_c) {
	if(r_c == r) continue;
	assert(m[queen_int(r_c, c, n)] == l_False);
      }
      // Check row
      for(ulong c_r = 0; c_r < n; ++ c_r) {
	if(c_r == c) continue;
	assert(m[queen_int(r, c_r, n)] == l_False);
      }
      // Check right diag
      long d_r = r - 1, d_c = c - 1;
      while(d_r >= 0 && d_c >= 0) {
	assert(m[queen_int(d_r, d_c, n)] == l_False);
	d_r--; d_c--;
      }
      d_r = r + 1; d_c = c + 1;
      while(d_r < n && d_c < n) {
	assert(m[queen_int(d_r, d_c, n)] == l_False);
	d_r++; d_c++;
      }
      
      // Check left diag
      d_r = r-1; d_c = c+1;
      while(d_r >= 0 && d_c < n) {
	assert(m[queen_int(d_r, d_c, n)] == l_False);
	d_r--; d_c++;
      }
      d_r = r+1; d_c = c-1;
      while(d_r < n && d_c >= 0) {
	assert(m[queen_int(d_r, d_c, n)] == l_False);
	d_r++; d_c--;
      }
    }
  }
  assert(queen_count == n);
}
