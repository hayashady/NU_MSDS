# import pulp
from pulp import LpVariable, LpProblem, LpMaximize, LpStatus, value, LpMinimize, GLPK
# Note, you may need to conda install or pip install GLPK

# Sensitivity Analysis File and Model (lp) file will output
# to your working directory.

# Problem (Theme Park Scheduling)
# define variables
S0000 = LpVariable("S0000", 0, None)
S0400 = LpVariable("S0400", 0, None)
S0800 = LpVariable("S0800", 0, None)
S1200 = LpVariable("S1200", 0, None)
S1600 = LpVariable("S1600", 0, None)
S2000 = LpVariable("S2000", 0, None)

# defines the problem
prob3 = LpProblem("problem", LpMinimize)
# Note, LpMaximize for a maximization problem, 
# and LpMinimize for a minimization problem

# define constraints
prob3 += S0000 + S2000 >= 90
prob3 += S0000 + S0400 >= 215
prob3 += S0400 + S0800 >= 250
prob3 += S0800 + S1200 >= 165
prob3 += S1200 + S1600 >= 300
prob3 += S1600 + S2000 >= 125

# Note, if <= then <=
# If >= then >=
# If = then ==

# define objective function
prob3 += S0000 + S0400 + S0800 + S1200 + S1600 + S2000

# solve the problem
prob3.writeLP("prob3.lp")
prob3.solve(GLPK(options=['--ranges prob3.sen']))
print ("Status:", LpStatus[prob3.status])

for v in prob3.variables():
    print(v.name, "=", v.varValue)

print ("Objective", value(prob3.objective))
print ("")

# Problem (Hot Dog)
# define variables
Beef = LpVariable("Beef", 0, None)
Pork = LpVariable("Pork", 0, None)
Chicken = LpVariable("Chicken", 0, None)
Turkey = LpVariable("Turkey", 0, None)

# defines the problem
prob4 = LpProblem("problem", LpMinimize)
# Note, LpMaximize for a maximization problem, 
# and LpMinimize for a minimization problem

# define constraints
prob4 += 640*Beef + 1055*Pork + 780*Chicken + 528*Turkey <= 100
prob4 += 32.5*Beef + 54*Pork + 25.6*Chicken + 6.4*Turkey <= 6
prob4 += 210*Beef + 205*Pork + 220*Chicken + 172*Turkey <= 27
prob4 += 0.75*Beef - 0.25*Pork - 0.25*Chicken - 0.25*Turkey >= 0
prob4 += -0.25*Beef + 0.75*Pork - 0.25*Chicken - 0.25*Turkey >= 0
prob4 += Beef + Pork + Chicken + Turkey >= 2/16

# Note, if <= then <=
# If >= then >=
# If = then ==

# define objective function
prob4 += 0.76*Beef + 0.82*Pork + 0.64*Chicken + 0.58*Turkey

# solve the problem
prob4.writeLP("prob4.lp")
prob4.solve(GLPK(options=['--ranges prob4.sen']))
print ("Status:", LpStatus[prob4.status])

for v in prob4.variables():
    print(v.name, "=", v.varValue)

print ("Objective", value(prob4.objective))