# Libraries ----
library(dplyr)
library(ggplot2)
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/theme_swd.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/control_graph.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/make_simulation.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/check_inputs.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/get_formula.R")

# Main ----
simulator <- function(params){
  status_inputs <- check_inputs(params)
  if(status_inputs[['status']] != "OK"){
    return(status_inputs$value)
  }else{
    info_to_simulate <- get_formula(params)
    class(params) <- c(info_to_simulate[['simulate_type']],
                       info_to_simulate[['axis_x']])
    
    xxz=make_simulation(params = params, 
                        info = info_to_simulate)
  }
  return(xxz)
}


params = list(years = NULL,
              var = NULL,
              start = 10000,
              monthly_contribution = NULL,
              total_money = 30000
              )

x=simulator(params)
x
