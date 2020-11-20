input_rules <- fromJSON(file = "C:/Users/55169/Desktop/Dev/investment-simulator/check_input_value.json")
control_graph <- function(params, info){
  label_y = info[['label_y']]
  UseMethod("control_graph")
}
control_graph.years <- function(params, info){
  control <- input_rules$years 
  axis_x = seq(control$min, control$max, by = 1)
  label_x = "Anos"
  label_title = paste(label_y, "vs", label_x)
  
  return(list(label_y = label_y,
              label_x = label_x,
              label_title = label_title,
              axis_x_calculate = axis_x,
              axis_x = axis_x))
}
control_graph.var <- function(params, info){
  control <- input_rules$var 
  # Control infinite values
  if(control$min == 0){
    control$min <- 0.001
  }
  axis_x = seq(control$min, control$max, by = 0.0015)*100
  label_x = "Rentabilidade ao mês (em %)"
  label_title = paste(label_y, "vs", label_x)
  
  return(list(label_y = label_y,
              label_x = label_x,
              label_title = label_title,
              axis_x_calculate = axis_x/100,
              axis_x = axis_x))
}
control_graph.start <- function(params, info){
  control <- input_rules$start 
  axis_x = seq(control$min, control$max, by = 500)
  label_x = "Entrada"
  label_title = paste(label_y, "vs", label_x)
  
  return(list(label_y = label_y,
              label_x = label_x,
              label_title = label_title,
              axis_x_calculate = axis_x,
              axis_x = axis_x))
}
control_graph.monthly_contribution <- function(params, info){
  control <- input_rules$monthly_contribution 
  axis_x = seq(control$min, control$max, by = 500)
  label_x = "Aporte mensal"
  label_title = paste(label_y, "vs", label_x)
  
  return(list(label_y = label_y,
              label_x = label_x,
              label_title = label_title,
              axis_x_calculate = axis_x,
              axis_x = axis_x))
}
