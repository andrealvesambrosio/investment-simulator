make_simulation <- function(params, ...){
  UseMethod("make_simulation")
}
make_simulation.single <- function(params, info){
  value = info[['simulate_formula']](params)
  if(value < 0){
    print("Revise a entrada, existem simulações que não fazem sentido..")
  }
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
    geom_point(color = "#d7385e") +
    geom_line(color = "#d7385e") +
    #geom_bar(stat = "identity", position = "dodge") +
    labs(y = control[['label_y']],
         x = control[['label_x']],
         title = control[['label_title']]) +
    theme_swd()
  
  return(plot)
}
