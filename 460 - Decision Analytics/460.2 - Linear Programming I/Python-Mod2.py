# import pulp
from pulp import LpVariable, LpProblem, LpMaximize, LpStatus, value, LpMinimize

# Problem (Initial)
# define variables
MFR = LpVariable("MFR", 0, None) # MFR>=0
EOC = LpVariable("EOC", 0, None) # EOC>=0
ER = LpVariable("ER", 0, None) # ER>=0
SC = LpVariable("SC", 0, None) # SC>=0
WM = LpVariable("WM", 0, None) # WM>=0

# defines the problem
prob = LpProblem("problem", LpMaximize)
# Note, LpMaximize for a maximization problem, 
# and LpMinimize for a minimization problem

# define constraints
prob += MFR    <= 400000
prob += EOC    <= 200000
prob += ER     <= 350000
prob += SC     <= 250000
prob += WM     <= 150000
prob += MFR + EOC + ER + SC + WM <= 1000000
prob += 0.4*MFR + 0.4*EOC + 0.4*ER - 0.6*SC - 0.6*WM >= 0

# Note, if <= then <=
# If >= then >=
# If = then ==

# define objective function
prob += 0.041*MFR + 0.008*EOC + 0.028*ER + 0.021*SC + 0.012*WM

# solve the problem
status = prob.solve()
print(f"Problem")
print(f"status={LpStatus[status]}")

# print the results
for variable in prob.variables():
    print(f"{variable.name} = {variable.varValue}")
    
print(f"Objective = {value(prob.objective)}")
print(f"")



















# Problem (Change)
# define variables
MFR = LpVariable("MFR", 0, 400000) # 0<=MFR<=400,000
EOC = LpVariable("EOC", 0, 200000) # 0<=EOC<=200,000
ER = LpVariable("ER", 0, 350000) # 0<=ER<=350,000
SC = LpVariable("SC", 0, 250000) # 0<=SC<=250,000
WM = LpVariable("WM", 0, 150000) # 0<=WM<=150,000

# defines the problem
prob2 = LpProblem("problem", LpMaximize)
# Note, LpMaximize for a maximization problem, 
# and LpMinimize for a minimization problem

# define constraints
prob2 += MFR + EOC + ER + SC + WM <= 1000000
prob2 += 0.4*MFR + 0.4*EOC + 0.4*ER - 0.6*SC - 0.6*WM >= 0

# Note, if <= then <=
# If >= then >=
# If = then ==

# define objective function
prob2 += 0.041*MFR + 0.008*EOC + 0.028*ER + 0.021*SC + 0.012*WM

# solve the problem
status = prob2.solve()
print(f"Problem")
print(f"status={LpStatus[status]}")

# print the results
for variable in prob2.variables():
    print(f"{variable.name} = {variable.varValue}")
    
print(f"Objective = {value(prob.objective)}")
print(f"")