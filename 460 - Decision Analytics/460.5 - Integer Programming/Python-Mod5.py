# import pulp
from pulp import LpVariable, LpProblem, LpMaximize, LpStatus, value, LpMinimize

# Problem (Knapsack Initial)
# define variables
x1 = LpVariable("x1", 0, 1, cat = 'Integer')
x2 = LpVariable("x2", 0, 1, cat = 'Integer')
x3 = LpVariable("x3", 0, 1, cat = 'Integer')
x4 = LpVariable("x4", 0, 1, cat = 'Integer')

# defines the problem
prob = LpProblem("problem", LpMaximize)
# Note, LpMaximize for a maximization problem, 
# and LpMinimize for a minimization problem

# define constraints
prob += 4*x1 + 5*x2 + 7*x3 + 3*x4 <= 14

# Note, if <= then <=
# If >= then >=
# If = then ==

# define objective function
prob += 12*x1 + 16*x2 + 22*x3 + 8*x4

# solve the problem
status = prob.solve()
print(f"Problem")
print(f"status={LpStatus[status]}")

# print the results
for variable in prob.variables():
    print(f"{variable.name} = {variable.varValue}")
    
print(f"Objective = {value(prob.objective)}")
print(f"")

# Problem (Racecar)
# define variables
x1 = LpVariable("x1", 0, 1, cat = 'Integer')
x2 = LpVariable("x2", 0, 1, cat = 'Integer')
x3 = LpVariable("x3", 0, 1, cat = 'Integer')
x4 = LpVariable("x4", 0, 1, cat = 'Integer')
x5 = LpVariable("x5", 0, 1, cat = 'Integer')
x6 = LpVariable("x6", 0, 1, cat = 'Integer')

# defines the problem
prob = LpProblem("problem", LpMaximize)
# Note, LpMaximize for a maximization problem, 
# and LpMinimize for a minimization problem

# define constraints
prob += 10200*x1 + 6000*x2 + 23800*x3 + 11100*x4 + 9800*x5 + 31600*x6 <= 35000

# Note, if <= then <=
# If >= then >=
# If = then ==

# define objective function
prob += 8*x1 + 3*x2 + 15*x3 + 7*x4 + 10*x5 + 12*x6

# solve the problem
status = prob.solve()
print(f"Problem")
print(f"status={LpStatus[status]}")

# print the results
for variable in prob.variables():
    print(f"{variable.name} = {variable.varValue}")
    
print(f"Objective = {value(prob.objective)}")
print(f"")