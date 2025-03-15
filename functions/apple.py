import numpy as np
import matplotlib.pyplot as plt

np.random.seed(42)

# Set number of simulation
num_simulations = 1000000

# Define ranges per hectar per year for apple
apple_income_range = (3000, 60000)
apple_costs_range = (15000, 30000)

# Generate random values for apple 
apple_income = np.random.uniform(low=apple_income_range[0], high=apple_income_range[1], size=num_simulations)
apple_costs = np.random.uniform(low=apple_costs_range[0], high=apple_costs_range[1], size=num_simulations)

# Apple profit
apple_profits = apple_income - apple_costs

# Plot apple profits
plt.hist(apple_profits, bins=50, alpha=0.7, label='Apple Profits')

# Add vertical lines for the quantiles
quantiles = np.quantile(apple_profits, [0.1, 0.5, 0.9])
for quantile in quantiles:
    plt.axvline(quantile, color='red', linestyle='dashed', linewidth=2)

# Define ranges per ha per year for sheep
sheep_income_range = (2000, 5000)
sheep_costs_range = (1000, 2500)

# Generate random values for sheep income and costs
sheep_income = np.random.uniform(low=sheep_income_range[0], high=sheep_income_range[1], size=num_simulations)
sheep_costs = np.random.uniform(low=sheep_costs_range[0], high=sheep_costs_range[1], size=num_simulations)

# Calculate sheep profits
sheep_profits = sheep_income - sheep_costs

# Calculate total profits
total_profits = apple_profits + sheep_profits

# Plot histograms for total profits, sheep profits, and apple profits
plt.figure()
plt.hist(total_profits, bins=50, alpha=0.5, label='Total Profits', color='white', edgecolor='black')
plt.hist(sheep_profits, bins=50, alpha=0.5, label='Sheep Profits', color='black', edgecolor='black')
plt.hist(apple_profits, bins=50, alpha=0.5, label='Apple Profits', color='pink', edgecolor='black')

# Add labels and legend
plt.xlabel('Profits (Euros)')
plt.ylabel('Frequency')
plt.legend()

# Show the plots
plt.show()