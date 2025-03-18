# set.seed(42)
# Monte Carlo Simulation
num_simulations <- 1000

# Define ranges per ha per year Euro
# lower and upper

apple_income <- runif(n = num_simulations, 
                      min = 3000, 
                      max = 60000)

apple_costs <- runif(n = num_simulations, 
                      min = 15000, 
                     max = 30000)

apple_profits <- apple_income - apple_costs

# Add sheep to the horticulture system 
# Now it is silvopastoral 

# Euro per ha per year

sheep_income <- runif(n = num_simulations, 
                      min = 2000, 
                      max = 5000)

sheep_costs <- runif(n = num_simulations, 
                     min = 1000, 
                     max = 2500)

sheep_profits <- sheep_income - sheep_costs

total_profits <- apple_profits + sheep_profits

# hist(total_profits, col = "white")
# hist(sheep_profits, add = TRUE, col = "black")
# hist(apple_profits, add = TRUE, col = "pink")
