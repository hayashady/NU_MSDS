# import pulp
from pulp import LpVariable, LpProblem, LpMaximize, LpStatus, value, LpMinimize, GLPK
# Note, you may need to conda install or pip install GLPK

# Sensitivity Analysis File and Model (lp) file will output
# to your working directory.

# Problem (Wine Problem)
# define variables
A1 = LpVariable("A1", 0, None)
A2 = LpVariable("A2", 0, None)
A3 = LpVariable("A3", 0, None)
A4 = LpVariable("A4", 0, None)
B1 = LpVariable("B1", 0, None)
B2 = LpVariable("B2", 0, None)
B3 = LpVariable("B3", 0, None)
B4 = LpVariable("B4", 0, None)

# defines the problem
prob4 = LpProblem("problem", LpMaximize)
# Note, LpMaximize for a maximization problem, 
# and LpMinimize for a minimization problem

# define constraints
prob4 += A1+A2+A3+A4 <= 3500
prob4 += B1+B2+B3+B4 <= 3100
prob4 += A1+B1 <= 1800
prob4 += A2+B2 <= 2300
prob4 += A3+B3 <= 1250
prob4 += A4+B4 <= 1750

# Note, if <= then <=
# If >= then >=
# If = then ==

# define objective function
prob4 += 39*A1+36*A2+34*A3+32*A4+32*B1+36*B2+37*B3+29*B4

# solve the problem
prob4.writeLP("prob4.lp")
prob4.solve(GLPK(options=['--ranges prob4.sen']))
print ("Status:", LpStatus[prob4.status])

# Note, we are only able to get sensitivity information because we are solving
# as a linear program.  If we solved as an Integer Program, then no 
# sensitivity information would be available.

for v in prob4.variables():
    print(v.name, "=", v.varValue)

print ("Objective", value(prob4.objective))
print ("")
