# Mark S. Baranowski
# Originally written as the solution to an extra credit assignment for CS 2100,
# now reincarnated as a submission for CS 6110.

# Solves a KenKen puzzle using Z3.

#! /usr/bin/env python
from z3 import *
import sys
import string

OPS = {'g', '+', '*', '/', '-'}

def solve_puzzle(n, puzzle):

  # Declare the grid for Z3
  grid = [[Int("X{0}_{1}".format(i, j)) for i in range(n)] for j in range(n)]
  
  # Create a solver instance for Z3
  s = Solver()

  # Add constraint that each cell is in the interval [1,n]
  for i in range(n):
    for j in range(n):
      s.add(And(1 <= grid[i][j], grid[i][j] <= n))
      
  # Add rule for rows having distinct values
  for r in range(n):
    s.add(Distinct([grid[r][c] for c in range(n)]))

  # Add rule for columns being distinct
  for c in range(n):
    s.add(Distinct([grid[r][c] for r in range(n)]))


  # Give Z3 our puzzle
  for cage in puzzle:
    goal = cage[0]
    op = cage[1]
    cells = [[cage[i],cage[i+1]] for i in range(2, len(cage), 2)]

    if op == 'g':
      s.add(goal == grid[cells[0][0]][cells[0][1]])

    if op == '+':
      sum = Sum([grid[cells[i][0]][cells[i][1]] for i in range(len(cells))])
      s.add(goal == sum)

    if op == '*':
      p = Product([grid[cells[i][0]][cells[i][1]] for i in range(len(cells))])
      s.add(goal == p)

    if op == '/':
      a = grid[cells[0][0]][cells[0][1]]
      b = grid[cells[1][0]][cells[1][1]]
      s.add(Or(goal == a/b, goal == b/a))

    if op == '-':
      a = grid[cells[0][0]][cells[0][1]]
      b = grid[cells[1][0]][cells[1][1]]
      s.add(Or(goal == a-b, goal == b-a))

  # Check if Z3 can find a solution
  if s.check() == sat:
    # Get Z3's solution
    m = s.model()
    # Extract the solution into a python matrix
    solution = [[0 for i in range(n)] for j in range(n)]
    for i in range(n):
      for j in range(n):
        solution[i][j] = int(str(m.eval(grid[i][j])))

    # Print solution
    print("\n".join([" ".join(map(lambda x:str(x),row)) for row in solution]))
  else:
    print("No solution")


if __name__ == '__main__':
  if len(sys.argv) == 1:
    n = 6

    puzzle=[[11, '+', 0, 0, 1, 0],
            [2,  '/', 0, 1, 0, 2],
            [3,  '-', 1, 1, 1, 2],
            [20, '*', 0, 3, 1, 3],
            [6,  '*', 0, 4, 0, 5, 1, 5, 2, 5],
            [3,  '/', 1, 4, 2, 4],
            [240,'*', 2, 0, 2, 1, 3, 0, 3, 1],
            [6,  '*', 2, 2, 2, 3],
            [6,  '*', 3, 2, 4, 2], 
            [7,  '+', 3, 3, 4, 3, 4, 4],
            [30, '*', 3, 4, 3, 5],
            [6,  '*', 4, 0, 4, 1],
            [9,  '+', 4, 5, 5, 5],
            [8,  '+', 5, 0, 5, 1, 5, 2],
            [2,  '/', 5, 3, 5, 4]]
    solve_puzzle(n, puzzle)
  else:
    # haphazard data parsing
    # * doesn't check for correct format

    # grab data and get out
    with open(sys.argv[1], 'r') as f:
      lines = [line.strip() for line in f]
    
    # remove blank lines
    lines = filter(lambda x: x != "", lines)
    
    # get n
    n = int(lines[0])

    # split rest of the lines
    lines = map(lambda x: x.split(), lines[1:])

    # cast strings to ints, except for OPS
    for i in range(len(lines)):
      lines[i] = map(lambda item: item if item in OPS else int(item), lines[i])

    solve_puzzle(n, lines)

