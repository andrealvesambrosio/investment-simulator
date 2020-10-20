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
  
  plot <- df %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(color = "#d7385e") +
    geom_line(color = "#d7385e") +
    #geom_bar(stat = "identity", position = "dodge") +
    labs(y = control[['label_y']],
         x = control[['label_x']],
         title = control[['label_title']]) +
    theme_swd()
  
  status <- NULL
  return(list(value = plot,
              status = status))
}
