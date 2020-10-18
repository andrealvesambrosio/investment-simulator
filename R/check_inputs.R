check_inputs <- function(params){
  qtd_input <- params %>%
    purrr::map(~length(.)) %>%
    unlist() %>%
    sum()
  
  # Other conditions: all > 0
  
  # Check if has at least 3 inputs
  if(qtd_input < 3){
    value <- "Por favor, preencha pelo menos tres campos."
    status = "erro"
    
    # Check if has at least one input missing to simulate
  } else if(qtd_input == 5){
    value <- "Por favor, deixe uma ou duas variaveis em branco. Senão, não há o que calcular"
    status = "erro"
    
    # Check if the relation isn't Anos vs Rentabilidade
  } else if(is.null(params[['var']]) & is.null(params[['years']])){
    value <- "Por favor, nao deixe Anos e Rentabilidade em branco ao mesmo tempo."
    status = "erro"
    
    # Check if the single simulate isn't Anos
  } else if(qtd_input == 4 & is.null(params[['years']])){
    value <- "Por favor, nao deixe apenas o campo Anos em branco."
    status = "erro"
    
    # Check if the single simulate isn't Rentabilidade
  } else if(qtd_input == 4 & is.null(params[['var']])){
    value <- "Por favor, nao deixe apenas o campo Rentabilidade em branco."
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
    }else{
      value <- "OK"
      status <- "OK"
    }
  }else{
    value <- "OK"
    status <- "OK"
  }
  return(list(value = value,
              status = status))
}
