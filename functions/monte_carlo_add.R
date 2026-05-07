library(decisionSupport)

make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

# Apple sheep estimates

input_estimates <- data.frame(
  variable = c("sheep_income", "sheep_cost", "apple_income", "apple_cost", "discount_rate", "n_years", "cost_orchard_establishment", "time_to_harvest"),
  lower = c(3000, 1000, 30000, 15000, 10, 20, 50000, 4),
  median = NA,
  upper = c(5000, 2500, 60000, 30000, 10, 20, 70000, 6),
  distribution = c("posnorm", "posnorm", "posnorm", "posnorm", "const", "const", "posnorm", 'unif'),
  label = c("Income from sheep (euro/year)", "Cost of sheep (euro/year)", "Income from apple (euro/year)", "Cost of apple (euro/year)", 
            "Discount Rate", "Simulated Years", "Construction cost of orchard", "Time until fruit harvest")
)

make_variables(as.estimate(input_estimates))

# apple_sheep_model_function

#things to add: time horizon. first establish orchards, no harvest for some time and then increasing yields



model_function <- function() {
  
  #round timne to harvest to next closest number (because it needs to be integer)
  time_to_harvest <- round(time_to_harvest, digits = 0)
  
  #turn variables into vector
  apple_income <- rep(apple_income, n_years)
  apple_cost <- rep(apple_cost, n_years)
  sheep_cost <- rep(sheep_cost, n_years)
  sheep_income <- rep(sheep_income, n_years)
  
  #adjust apple production benefit: yield only after x years
  apple_income[1:time_to_harvest] <- 0
  
  #in first year of production there are construction costs for trees
  apple_cost[1] <- cost_orchard_establishment
  
  #also turn sheep costs into vector( even though it does not change)
 
  #rest of model can stay the same
  AF_income <- sheep_income + apple_income
  AF_cost <- sheep_cost + apple_cost
  AF_final_result <- AF_income - AF_cost
  sheep_only <- sheep_income - sheep_cost
  Decision_benefit <- AF_final_result - sheep_only
  
  AF_NPV <- discount(AF_final_result, discount_rate = discount_rate, calculate_NPV = TRUE)
  NPV_sheep_only <- discount(sheep_only, discount_rate = discount_rate, calculate_NPV = TRUE)
  NPV_decision <- discount(Decision_benefit, discount_rate = discount_rate, calculate_NPV = TRUE)
  
  return(list(
    NPV_Agroforestry_System = AF_NPV,
    NPV_Treeless_System = NPV_sheep_only,
    NPV_decision = NPV_decision
  ))
}

model_function()

# apple_sheep_mc

apple_sheep_mc_simulation <- mcSimulation(
  estimate = as.estimate(input_estimates),
  model_function = model_function,
  numberOfModelRuns = 100,
  functionSyntax = "plainNames"
)
