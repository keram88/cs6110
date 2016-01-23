#! /usr/bin/env python3

import z3

from z3 import And as And
from z3 import Or as Or
from z3 import Not as Not

import sys

def rotate_back(p, n):
  i,j = p
  return (j, n - i - 1)

def rotate(i, j, n, times, v):
  p = (i, j)
  for i in range(0, times):
    p = rotate_back(p,n)
  i, j = p
  return v[i][j]

def irange(start, end):
  for i in range(start, end+1):
    yield i

def variables(n):
  return [[z3.Bool("q_{}_{}".format(i, j)) for j in irange(1, n)] for i in irange(1, n)]

def not_col(i, j, n, v):
  return Or([v[k][j] if k!=i else False for k in range(0, n)])

def col(i, j, n, v):
  return And([Not(v[k][j]) if k!=i else True for k in range(0, n)])

def or_col(i, j, n, v, I):
  return [Or(Not(v[k][j]), I) if k!=i else True for k in range(0, n)]

def not_diagr(i, j, n, v):
  return Or([v[i+k][j+k] for k in range(1, min(n-i, n-j))])

def diagr(i, j, n, v):
  return And([Not(v[i+k][j+k]) for k in range(1, min(n-i, n-j))])

def or_diagr(i, j, n, v, I):
  return [Or(Not(v[i+k][j+k]), I) for k in range(1, min(n-i, n-j))]

def diagl(i, j, n, v):
  return And([Not(v[i+k][j-k]) for k in range(1, min(j+1, n-i))])

def or_diagl(i, j, n, v, I):
  return [Or(Not(v[i+k][j-k]), I) for k in range(1, min(j+1, n-i))]

def not_diagl(i, j, n, v):
  return Or([v[i+k][j-k] for k in range(1, min(j+1, n-i))])

def print_answers(v, n, model):
  out="\n".join(("".join(("X" if
                           str(model[v[i][j]]) == 'True' else
                           "." for j in range(0,n)))
                 for i in range(0,n)))
  print()
  print(out)
  
def main(n):
  v = variables(n)
  S = z3.Solver()
  impls = [[z3.Bool("I_{}_{}".format(i,j)) for j in range(1,n+1)] for i in range(1,n+1)]
  for i in range(0,n):
    for j in range(0,n):
#      S.add(Or(Not(impls[i][j]), And(v[i][j], col(i,j,n,v), diagr(i,j,n,v), diagl(i,j,n,v))))
      S.add(Or(Not(impls[i][j]), v[i][j]))
      S.add(And(or_col(i, j, n, v, Not(impls[i][j]))),
            And(or_diagr(i,j,n,v, Not(impls[i][j]))),
            And(or_diagl(i,j,n,v, Not(impls[i][j]))))
#      S.add(Or(Not(v[i][j]),
#               not_col(i,j,n,v),
#               not_diagr(i,j,n,v),
#               not_diagl(i,j,n,v),
#               impls[i][j]))
    S.add(Or([impls[i][j] for j in range(n)]))
    
#    S.add(Or([And(v[i][j],
#                  col(i,j,n,v),
#                  diagr(i,j,n,v), diagl(i,j,n,v)) for j in range(0,n)]))
  solns = 0
  print(S)
  while S.check() == z3.sat:
    x = S.model()
    print_answers(v, n, x)
    S.add(Or([Or([Not(v[i][j]) if
                  str(x[v[i][j]]) == 'True' else
                  v[i][j] for i in range(0,n)])
              for j in range(0,n)]))
    # Flip vertical
    S.add(Or([Or([Not(v[n-i-1][j]) if
                  str(x[v[i][j]]) == 'True' else
                  v[n-i-1][j] for i in range(0,n)])
              for j in range(0,n)]))
    # Flip horizontal
    S.add(Or([Or([Not(v[i][n-j-1]) if
                  str(x[v[i][j]]) == 'True' else
                  v[i][n-j-1] for i in range(0,n)])
              for j in range(0,n)]))
    # Flip non-main diag
    S.add(Or([Or([Not(v[j][i]) if
                  str(x[v[i][j]]) == 'True' else
                  v[j][i] for i in range(0,n)])
              for j in range(0,n)]))
    # Flip main diag
    S.add(Or([Or([Not(v[n-j-1][n-i-1]) if
                  str(x[v[i][j]]) == 'True' else
                  v[n-j-1][n-i-1] for i in range(0,n)])
              for j in range(0,n)]))
    # Rotations
    for times in range(1,4):
      S.add(Or([Or([Not(rotate(i, j, n, times, v)) if
                    str(x[v[i][j]]) == 'True' else
                    rotate(i, j, n, times, v) for i in range(0,n)])
                for j in range(0,n)]))
      
    solns += 1
  else:
    print(solns)

if __name__ == '__main__':
  if len(sys.argv) > 1:
    main(int(sys.argv[1]))
  else:
    print("Plz enter a number")
