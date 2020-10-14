# Libraries ----
library(dplyr)

# Auxiliary ----
get_formula <- function(params){
  # Check which inputs are null
  null_input <- c()
  for(input in names(params)){
    if(is.null(params[[input]])){
      null_input <- append(null_input, input)
    }
  }
  
  # Total money expression
  if("total_money" %in% null_input){
    simulate_formula <- function(params){
      params[['start']]*(1 + params[['var']])^(12*params[['years']]) + 
        params[['monthly_contribution']]*(((1 + params[['var']])^(12*params[['years']])) - 1)/params[['var']]
    }
    main_input <- "total_money"
      
  # Monthly Contribution expression
  }else if("monthly_contribution" %in% null_input){
    simulate_formula <- function(params){
      (params[['total_money']] - 
         params[['start']]*(1 + params[['var']])^(12*params[['years']]))/((((1 + params[['var']])^(12*params[['years']])) - 1)/params[['var']])
    }
    main_input <- "monthly_contribution"
  
  # Start Money expression
  }else if("start" %in% null_input){
    simulate_formula <- function(params){
      (params[['total_money']] - 
         params[['monthly_contribution']]*(((1 + params[['var']])^(12*params[['years']])) - 1)/params[['var']])/((1+params[['var']])^(12*params[['years']]))
    }
    main_input <- "start"
  }
  
  # Check what simulating type we need + axis_x if needed
  if(length(null_input) == 1){
    simulate_type = "single"
    axis_x = NULL
  } else{
    simulate_type = "graph"
    axis_x = null_input[null_input != main_input]
  }
  return(list(simulate_formula = simulate_formula,
              null_inputs = null_input,
              main_input = main_input,
              axis_x = axis_x,
              simulate_type = simulate_type))
}
check_inputs <- function(params){
  qtd_input <- params %>%
    purrr::map(~length(.)) %>%
    unlist() %>%
    sum()
  
  # Other conditions: all > 0
  
  # Check if has at least 3 inputs
  if(qtd_input < 3){
    value <- "Por favor, preencha pelo menos três campos."
    status = "erro"
    
    # Check if has at least one input missing to simulate
  } else if(qtd_input == 5){
      value <- "Por favor, deixe uma ou duas variáveis em branco. Senão, não há o que calcular"
      status = "erro"
    
    # Check if the relation isn't Anos vs Rentabilidade
  } else if(is.null(params[['var']]) & is.null(params[['years']])){
      value <- "Por favor, não deixe Anos e Rentabilidade em branco ao mesmo tempo."
      status = "erro"
    
    # Check if the single simulate isn't Anos
  } else if(qtd_input == 4 & is.null(params[['years']])){
      value <- "Por favor, não deixe apenas o campo Anos em branco."
      status = "erro"
    
    # Check if the single simulate isn't Rentabilidade
  } else if(qtd_input == 4 & is.null(params[['var']])){
      value <- "Por favor, não deixe apenas o campo Rentabilidade em branco."
      status = "erro"
    
    # Check if Total Money < Start
  } else if((!is.null(params[['total_money']]) & 
             !is.null(params[['start']]))){
      if(params[['total_money']] <= params[['start']]){
        value <- "Por favor, o Montante final precisa ser maior que a Entrada."
        status = "erro"
  }
    
    # Check if Total Money < Monthly Contribution
  } else if((!is.null(params[['total_money']]) & 
             !is.null(params[['monthly_contribution']]))){
    if(params[['total_money']] <= params[['monthly_contribution']]){
      value <- "Por favor, o Montante final precisa ser maior que a Entrada."
      status = "erro"
    }
  }else{
    value <- "OK"
    status <- "OK"
  }
  return(list(value = value,
              status = status))
}

do_simulation <- function(params, ...){
  UseMethod("do_simulation")
}
do_simulation.single = function(params, simulate_function){
  value = info_to_simulate[['simulate_formula']](params)
  return(value)
}
do_simulation.graph <- function(params, simulate_function){
  # IFs to create the labels, the sequences, ...
}


# Main ----
params = list(years = NULL,
              var = 0.05,
              start = 5000,
              monthly_contribution = 1000,
              total_money = NULL
              )


check_inputs(params)
get_formula(params)
simulator(params)

simulator <- function(params){
  status_inputs <- check_inputs(params)
  if(status_inputs[['status']] != "OK"){
    return("Algo errado nos inputs")
  }else{
    info_to_simulate <- get_formula(params)
    class(params) <- info_to_simulate[['simulate_type']]
    
    xxz=do_simulation(params = params, 
                    simulate_function = info_to_simulate)
  }
}


# Tempo > Rentabilidade > Entrada > Aporte > Montante 

# 1. Montante
# 2. Aporte
# 3. Entrada

# 1. Tempo vs Aporte
# 2. Tempo vs Entrada
# 3. Tempo vs Montante

# 4. Rentabilidade vs Aporte
# 5. Rentabilidade vs Entrada
# 6. Rentabilidade vs Montante

# 7. Entrada vs Aporte
# 8. Entrada vs Montante

# 9. Aporte vs Montante
