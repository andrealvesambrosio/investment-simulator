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
