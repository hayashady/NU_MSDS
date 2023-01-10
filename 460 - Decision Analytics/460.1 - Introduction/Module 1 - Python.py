# import pulp
from pulp import LpVariable, LpProblem, LpMaximize, LpStatus, value, LpMinimize

# Problem 1
# define variables
Tables = LpVariable("Tables", 0, None) # Tables>=0
Chairs = LpVariable("Chairs", 0, None) # Chairs>=0

# defines the problem
prob = LpProblem("problem", LpMaximize)
# Note, LpMaximize for a maximization problem, 
# and LpMinimize for a minimization problem

# define constraints
prob += 2*Tables    +  1*Chairs    <= 12
prob += 2*Tables    +  2*Chairs    <= 18
# Note, if <= then <=
# If >= then >=
# If = then ==

# define objective function
prob += 16*Tables   + 10*Chairs

# solve the problem
status = prob.solve()
print(f"Problem 1")
print(f"status={LpStatus[status]}")

# print the results
for variable in prob.variables():
    print(f"{variable.name} = {variable.varValue}")
    
print(f"Objective = {value(prob.objective)}")
print(f"")

# Problem 2
# define variables
Tables = LpVariable("Tables", 0, None) # Tables>=0
Chairs = LpVariable("Chairs", 0, None) # Chairs>=0
Coffee = LpVariable("Coffee", 0, None) # Coffee>=0

# defines the problem
prob = LpProblem("problem", LpMaximize)
# Note, LpMaximize for a maximization problem, 
# and LpMinimize for a minimization problem

# define constraints
prob += 2*Tables    +  1*Chairs     +  2*Coffee    <= 12
prob += 2*Tables    +  2*Chairs     +  1*Coffee    <= 18
# Note, if <= then <=
# If >= then >=
# If = then ==

# define objective function
prob += 16*Tables   + 10*Chairs     + 15*Coffee

# solve the problem
status = prob.solve()
print(f"Problem 2")
print(f"status={LpStatus[status]}")

# print the results
for variable in prob.variables():
    print(f"{variable.name} = {variable.varValue}")
    
print(f"Objective = {value(prob.objective)}")
print(f"")

# Problem 3
# define variables
Tables = LpVariable("Tables", 0, None) # Tables>=0
Chairs = LpVariable("Chairs", 0, None) # Chairs>=0
Coffee = LpVariable("Coffee", 0, None) # Coffee>=0

# defines the problem
prob = LpProblem("problem", LpMaximize)
# Note, LpMaximize for a maximization problem, 
# and LpMinimize for a minimization problem

# define constraints
prob += 2*Tables    +  1*Chairs     +  2*Coffee    <= 12
prob += 2*Tables    +  2*Chairs     +  1*Coffee    <= 18
# Note, if <= then <=
# If >= then >=
# If = then ==

# define objective function
prob += 15*Tables   + 9*Chairs     + 13.5*Coffee

# solve the problem
status = prob.solve()
print(f"Problem 3")
print(f"status={LpStatus[status]}")

# print the results
for variable in prob.variables():
    print(f"{variable.name} = {variable.varValue}")
    
print(f"Objective = {value(prob.objective)}")
print(f"")
