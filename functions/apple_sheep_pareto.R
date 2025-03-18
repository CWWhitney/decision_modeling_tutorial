# Source the original model
source("functions/apples_sheep_in_class.R")

# Define management variables per hectare
max_stocking_density <- 20  # Max sheep per ha before overgrazing effects occur
stocking_density <- runif(num_simulations, min = 5, max = max_stocking_density)

max_tree_density <- 500   # Maximum trees per hectare
tree_density <- runif(num_simulations, min = 50, max = max_tree_density)

# Define proportion of tree area vs. sheep area
proportion_tree_area <- runif(num_simulations, min = 0.2, max = 0.8)
proportion_sheep_area <- 1 - proportion_tree_area  # Remaining area for sheep

# --- FINANCIAL CALCULATIONS ---
# Profits for agroforestry (trees + sheep)
apple_profits <- apple_income - apple_costs
sheep_profits <- sheep_income - sheep_costs

total_profits_agroforestry <- apple_profits + (sheep_profits * proportion_sheep_area)
total_profits_treeless <- sheep_profits  # No tree component in treeless system

# --- BIODIVERSITY CALCULATIONS ---
# Define species richness and grazing impact factors per hectare
species_richness_per_tree <- 0.05  # Each tree adds 0.05 species per ha
species_loss_per_sheep <- 0.02  # Each sheep reduces 0.02 species per ha

# Calculate biodiversity per ha
tree_species_richness <- tree_density * species_richness_per_tree
grazing_species_loss <- stocking_density * species_loss_per_sheep

# Compute biodiversity outcomes per ha
biodiversity_agroforestry <- tree_species_richness - grazing_species_loss
biodiversity_treeless <- -grazing_species_loss  # No trees, only grazing effect

# Calculate difference (decision impact of trees)
# measures the financial difference when switching to trees
total_profits <- total_profits_agroforestry - total_profits_treeless

# Calculate difference (decision impact of trees on biodiversity)
# measures the net gain/loss in biodiversity from switching to trees
biodiversity_impact <- biodiversity_agroforestry - biodiversity_treeless

# Define a weighting factor for biodiversity impact (convert biodiversity into € equivalent)
biodiversity_value_per_species <- 500  # Assume each additional species is worth €500

# Compute a combined utility function
utility <- total_profits + (biodiversity_impact * biodiversity_value_per_species)


# --- PARETO FRONT ANALYSIS ---
# Store results in dataframe
results <- data.frame(
  total_profits,
  biodiversity_impact
)

# Scatter plot
# plot(results$biodiversity_impact, results$total_profits,
#      col = "blue", pch = 19, xlab = "Biodiversity Impact (Species Change per ha)",
#      ylab = "Total Profits (€ per ha)", main = "Pareto Front: Agroforestry vs. Treeless System")

# finding the non-dominated points—points 
# where no other solution is better in both biodiversity and total profits

# A point (biodiversity, profit) is Pareto-optimal 
# if no other point has higher biodiversity 
# AND higher profits.

# Sort results by biodiversity impact
results <- results[order(results$biodiversity_impact, decreasing = TRUE),]

# Initialize Pareto front
pareto_front <- results[1, ]  # Start with the first point

# Loop through sorted results and keep only non-dominated points
for (i in 2:nrow(results)) {
  if (results$total_profits[i] > tail(pareto_front$total_profits, 1)) {
    pareto_front <- rbind(pareto_front, results[i, ])
  }
}

# # Plot all points
# plot(results$biodiversity_impact, results$total_profits,
#      col = "gray", pch = 19, xlab = "Biodiversity Impact (Species Change per ha)",
#      ylab = "Total Profits (€ per ha)", main = "Pareto Front: Agroforestry vs. Treeless System")
# 
# # Highlight Pareto front points
# points(pareto_front$biodiversity_impact, pareto_front$total_profits,
#        col = "red", pch = 19, type = "o", lwd = 2)


# zoom in on the Pareto-optimal points and examine the conditions for key operational variables:

# Stocking density (sheep per hectare)
# Tree density (trees per hectare)
# Proportion of tree area
# Proportion of sheep area (1 - proportion_tree_area)

# Extract operational conditions for Pareto-optimal points
pareto_conditions <- results[results$biodiversity_impact %in% pareto_front$biodiversity_impact &
                               results$total_profits %in% pareto_front$total_profits,]

# Merge with original dataset to include operational variables
pareto_conditions <- merge(pareto_conditions, data.frame(
  biodiversity_impact, total_profits, stocking_density, tree_density, proportion_tree_area),
  by = c("biodiversity_impact", "total_profits"))

# Show Pareto-optimal conditions
# print(pareto_conditions)

# Determine dynamic position based on plot coordinates
text_positions <- ifelse(
  pareto_front$biodiversity_impact > median(pareto_front$biodiversity_impact), 2, 4
)

# Adjust for very high or low points
text_positions <- ifelse(
  pareto_front$total_profits > quantile(pareto_front$total_profits, 0.75), 1, text_positions
)
text_positions <- ifelse(
  pareto_front$total_profits < quantile(pareto_front$total_profits, 0.25), 3, text_positions
)

# # Plot Pareto front
# plot(pareto_front$biodiversity_impact, pareto_front$total_profits,
#      col = "red", pch = 19, type = "o", lwd = 2,
#      xlab = "Biodiversity Impact (Species Change per ha)",
#      ylab = "Total Profits (€ per ha)", main = "Pareto Front: Agroforestry vs. Treeless System")
# 
# # Add footnote at the bottom
# mtext("S = Stocking Density (sheep/ha), T = Tree Density (trees/ha), P = Proportion of Tree Area", 
#       side = 1, line = 4, cex = 0.8)
# 
# # Create multi-line labels with line breaks
labels <- paste0("S:", round(pareto_conditions$stocking_density, 1), "\n",
                 "T:", round(pareto_conditions$tree_density, 1), "\n",
                 "P:", round(pareto_conditions$proportion_tree_area, 2))

# # Annotate with dynamically positioned text
# text(pareto_front$biodiversity_impact, pareto_front$total_profits,
#      labels = labels, pos = text_positions, cex = 0.8)
