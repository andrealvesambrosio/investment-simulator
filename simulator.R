# Libraries ----
library(dplyr)

# Auxiliary ----
get_formula <- function(x){
  
  final <- start*(1 + var)^months + mc*(((1 + var)^months) - 1)/var
  
  start <- function(final, mc, var, months){
    (final - mc*(((1 + var)^months) - 1)/var)/((1+var)^months)
  }
  
  mc <- function(final, start, var, months){
    (final - start*(1 + var)^months)/((((1 + var)^months) - 1)/var)
  }
  
}
final <- start*(1 + var)^months + mc*(((1 + var)^months) - 1)/var
start <- (final - mc*(((1 + var)^months) - 1)/var)/((1+var)^months)


params = list(start = 1000,
              years = NULL,
              monthly_contribution = 100,
              var = NULL,
              total_money = 1000)

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
    return(list(value = msg, status = "erro"))
    
  }else{
    start = params[['start']]
    months = 12*params[['years']]
    mc = params[['monthly_contribution']]
    var = params[['var']]
    final = params[['total_money']]
    
    final <- start*(1 + var)^months + mc*(((1 + var)^months) - 1)/var
    
    return(list(value = final, status = "single_simulate"))
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
