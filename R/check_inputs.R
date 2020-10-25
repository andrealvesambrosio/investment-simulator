interval <- function(obj, min, max, include_min){
  if(include_min == T){
    obj >= min & obj <= max
  }else{
    obj > min & obj <= max
  }
}

is_missing <- function(obj){
  if(is.null(obj)){
    return(TRUE)
  }else if(is.na(obj)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

check_inputs <- function(params, input_rules, supress = TRUE){
  message("Entrando no check_inputs")
  message(params)
  
  status <- "OK"
  value <- ""
  
  # Checking missing inputs
  qtd_NA <- params %>%
    is.na() %>%
    sum()
  if(qtd_NA > 0){
    status <- "erro"
    if(supress == FALSE){
      value <- "Por favor preencha todos os campos"
    }
    return(list(status = status,
                value = value))
  }
  
  # Checking if input are all numbers
  
  cond <- TRUE
  index <- 1
  while(cond){
    message("index: ", index)
    my_param <- names(input_rules)[index]
    my_rules <- input_rules[[my_param]]

    # Main input is from class NULL
    if(is.null(params[[my_param]])){
      cond <- TRUE
    }else{
      cond <- interval(params[[my_param]], 
                       min = my_rules[['min']],
                       max = my_rules[['max']],
                       include_min = my_rules[['include_min']])
    }
  
    if(cond == FALSE){
      status <- "erro"
      if(supress == FALSE){
        value <- my_rules[['value']]
      }
    }
    if(index == length(names(input_rules))){
      cond <- FALSE
    }
    index <- index + 1
  }
  return(list(value = value,
              status = status))
}
