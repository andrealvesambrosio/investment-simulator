# Libraries ----
library(dplyr)
library(ggplot2)
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
    label_y <- "Montante final"
      
  # Monthly Contribution expression
  }else if("monthly_contribution" %in% null_input){
    simulate_formula <- function(params){
      (params[['total_money']] - 
         params[['start']]*(1 + params[['var']])^(12*params[['years']]))/((((1 + params[['var']])^(12*params[['years']])) - 1)/params[['var']])
    }
    main_input <- "monthly_contribution"
    label_y <- "Aporte mensal"
  
  # Start Money expression
  }else if("start" %in% null_input){
    simulate_formula <- function(params){
      (params[['total_money']] - 
         params[['monthly_contribution']]*(((1 + params[['var']])^(12*params[['years']])) - 1)/params[['var']])/((1+params[['var']])^(12*params[['years']]))
    }
    main_input <- "start"
    label_y <- "Entrada"
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
              label_y = label_y,
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
      }else{
        value <- "OK"
        status <- "OK"
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
control_graph <- function(params, info, ...){
  label_y = info[['label_y']]
  UseMethod("control_graph")
}
control_graph.years <- function(params, info){
  axis_x = seq(1, 40, by = 1)
  label_x = "Anos"
  label_title = paste(label_y, "vs", label_x)
  
  return(list(label_y = label_y,
              label_x = label_x,
              label_title = label_title,
              axis_x_calculate = axis_x,
              axis_x = axis_x))
}
control_graph.var <- function(params, info){
  axis_x = seq(0.0025, 0.025, by = 0.0015)*100
  label_x = "Rentabilidade ao mês (em %)"
  label_title = paste(label_y, "vs", label_x)
  
  return(list(label_y = label_y,
              label_x = label_x,
              label_title = label_title,
              axis_x_calculate = axis_x/100,
              axis_x = axis_x))
}
control_graph.start <- function(params, info){
  axis_x = c(seq(100, 1000, by = 100),
             seq(1500, 5000, by = 500),
             seq(6000, 10000, by = 1000),
             seq(12500, 20000, by = 2500),
             seq(25000, 50000, by = 5000),
             seq(60000, 100000, by = 10000))
  
  label_x = "Entrada"
  label_title = paste(label_y, "vs", label_x)
  
  return(list(label_y = label_y,
              label_x = label_x,
              label_title = label_title,
              axis_x_calculate = axis_x,
              axis_x = axis_x))
}
control_graph.monthly_contribution <- function(params, info){
  axis_x = c(seq(100, 1000, by = 100),
             seq(1500, 5000, by = 500),
             seq(6000, 10000, by = 1000))
  
  label_x = "Aporte mensal"
  label_title = paste(label_y, "vs", label_x)
  
  return(list(label_y = label_y,
              label_x = label_x,
              label_title = label_title,
              axis_x_calculate = axis_x,
              axis_x = axis_x))
}

make_simulation <- function(params, ...){
  UseMethod("make_simulation")
}
make_simulation.single <- function(params, info){
  value = info[['simulate_formula']](params)
  return(value)
}
make_simulation.graph <- function(params, info){
  control <- control_graph(params, info)

  params[[info[['axis_x']]]] = control[['axis_x_calculate']]
  
  y = info[['simulate_formula']](params)
  
  df <- tibble::tibble(x = control[['axis_x']],
                       y = y) 
  
  plot <- df %>%
    ggplot(aes(x = x, y = y)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(y = control[['label_y']],
         x = control[['label_x']],
         title = control[['label_title']])
  
  return(plot)
}


# Main ----
params = list(years = 15,
              var = 0.0052,
              start = 0,
              monthly_contribution = NULL,
              total_money = 1000000
              )


check_inputs(params)
get_formula(params)
x=simulator(params)

simulator <- function(params){
  status_inputs <- check_inputs(params)
  if(status_inputs[['status']] != "OK"){
    return("Algo errado nos inputs")
  }else{
    info_to_simulate <- get_formula(params)
    class(params) <- c(info_to_simulate[['simulate_type']],
                       info_to_simulate[['axis_x']])
    
    xxz=make_simulation(params = params, 
                        info = info_to_simulate)
  }
  return(xxz)
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
