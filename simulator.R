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
    simulate_formula <- function(var, months, start, mc){
      start*(1 + var)^months + mc*(((1 + var)^months) - 1)/var
    }
    main_input <- "total_money"
  
  # Monthly Contribution expression
  }else if("monthly_contribution" %in% null_input){
    simulate_formula <- function(var, months, final, start){
      (final - start*(1 + var)^months)/((((1 + var)^months) - 1)/var)
    }
    main_input <- "monthly_contribution"
  
  # Start Money expression
  }else if("start" %in% null_input){
    simulate_formula <- function(var, months, final, mc){
      (final - mc*(((1 + var)^months) - 1)/var)/((1+var)^months)
    }
    main_input <- "start"
  }
  return(list(simulate_formula = simulate_formula,
              null_inputs = null_input,
              main_input = main_input))
}

# Main ----
params = list(years = 4,
              var = NULL,
              start = 0,
              monthly_contribution = 0,
              total_money = NULL
              )

get_formula(params)
simulator(params)

simulator <- function(params){
  
  qtd_input <- params %>%
    purrr::map(~length(.)) %>%
    unlist() %>%
    sum()
  
  # Other conditions: total_money > start, mc ... all > 0
  
  # Check if has at least 3 inputs
  if(qtd_input < 3){
    msg <- "Por favor, preencha pelo menos três campos."
    return(list(value = msg, 
                status = "erro"))
    
    # Check if has at least one input missing to simulate
  } else if(qtd_input == 5){
    msg <- "Por favor, deixe uma ou duas variáveis em branco. Senão, não há simulação"
    return(list(value = msg, 
                status = "erro"))
    
    # Check if the relation isn't Anos vs Rentabilidade
  } else if(is.null(params[['var']]) & is.null(params[['years']])){
    msg <- "Por favor, não deixe Anos e Rentabilidade em branco ao mesmo tempo."
    return(list(value = msg, 
                status = "erro"))
    
    # Check if the single simulate isn't Anos
  } else if(qtd_input == 4 & is.null(params[['years']])){
    msg <- "Por favor, não deixe apenas o campo Anos em branco."
    return(list(value = msg, 
                status = "erro"))
    
    # Check if the single simulate isn't Rentabilidade
  } else if(qtd_input == 4 & is.null(params[['var']])){
    msg <- "Por favor, não deixe apenas o campo Rentabilidade em branco."
    return(list(value = msg, 
                status = "erro"))
    
  }else{
    start = params[['start']]
    months = 12*params[['years']]
    mc = params[['monthly_contribution']]
    var = params[['var']]
    final = params[['total_money']]
    
    final <- start*(1 + var)^months + mc*(((1 + var)^months) - 1)/var
    
    return(list(value = final, 
                status = "single_simulate"))
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
