make_simulation <- function(params, ...){
  UseMethod("make_simulation")
}
make_simulation.single <- function(params, info){
  value <- info[['simulate_formula']](params)
  value <- round(value, digits = 2)
  
  if(value < 0){
    status <- ("Revise os valores fornecidos.")
  }else{
    status <- NULL
  }
  
  if(info[['main_input']] %in% c("start", "monthly_contribution", "total_money")){
    value_str <- paste0("R$ ", formatC(as.numeric(value), 
                                  format="f", 
                                  digits=2, 
                                  big.mark=".",
                                  decimal.mark = ","))
  }
  
  # Calculate investiment, profit, rentability
  start_ = params[['start']]
  monthly_contribution_ = params[['monthly_contribution']]
  total_money_ = params[['total_money']]
  years_ = params[['years']]
  
  eval(parse(text = paste0(info[['main_input']], "_ <- value")))
  investiment <- start_ + 12*years_*monthly_contribution_
  profit <- total_money_ - investiment
  rentability <- (paste0(round(100*(profit/investiment), 
                               digits = 2), 
                         "%"))
  
  investiment <- paste0("R$ ", formatC(as.numeric(investiment), 
                        format="f", 
                        digits=2, 
                        big.mark=".",
                        decimal.mark = ","))
  
  profit <- paste0("R$ ", formatC(as.numeric(profit), 
                                       format="f", 
                                       digits=2, 
                                       big.mark=".",
                                       decimal.mark = ","))
  
  
  return(list(value = value_str,
              value_raw = value,
              status = status,
              investiment = investiment,
              profit = profit,
              rentability = rentability))
}
make_simulation.graph <- function(params, info){
  control <- control_graph(params, info)
  
  params[[info[['axis_x']]]] = control[['axis_x_calculate']]
  
  y = info[['simulate_formula']](params)
  
  df <- tibble::tibble(x = control[['axis_x']],
                       y = y) 
  
  # Control the scale
  # Axis y
  log_control_y <- as.integer(log10(max(df$y)))
  if(between(x = log_control_y, left = 3, right = 5)){
    control_str_y <- "(em milhares)"
    df$y <- df$y/1e3
  }else if(log_control_y >= 6){
    control_str_y <- "(em milhões)"
    df$y <- df$y/1e6
  }else{
    control_str_y <- ""
  }
  
  # Axis x
  log_control_x <- as.integer(log10(max(df$x)))
  if(between(x = log_control_x, left = 3, right = 5)){
    control_str_x <- "(em milhares)"
    df$x <- df$x/1e3
  }else if(log_control_x >= 6){
    control_str_x <- "(em milhões)"
    df$x <- df$x/1e6
  }else{
    control_str_x <- ""
  }
  
  graph_info <- list(df = df,
                     label_y = paste(control[['label_y']], control_str_y),
                     label_x = paste(control[['label_x']], control_str_x),
                     label_title = control[['label_title']])

  status <- NULL
  return(list(value = graph_info,
              status = status))
}
