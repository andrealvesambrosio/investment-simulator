# Libraries ----
library(dplyr)
library(ggplot2)
library(rjson)
library(highcharter)

# Source ----
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/theme_swd.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/control_graph.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/make_simulation.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/check_inputs.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/get_formula.R")
input_rules <- rjson::fromJSON(file = "C:/Users/55169/Desktop/Dev/investment-simulator/check_input_value.json")

# Server ----
function(input, output, session) {
  
  # Simulator ----
  simulator <- function(params, supress = TRUE, qtd_var = 1){
    message("Entrou no simulator")
    status_inputs <- check_inputs(params, input_rules, 
                                  supress = supress, 
                                  qtd_null = qtd_var)
    message("Rodou o check_inputs")
    if(status_inputs[['status']] != "OK"){
      message("check_inputs deu erro\n")
      return(status_inputs)
    }else{
      message("check_inputs deu certo\n")
      info_to_simulate <- get_formula(params)
      #message("info_to_simulate")###
      #message(info_to_simulate)
      class(params) <- c(info_to_simulate[['simulate_type']],
                         info_to_simulate[['axis_x']])
      
      out <- make_simulation(params = params, 
                            info = info_to_simulate)
      
      #message("out")###
      #message(out)
      
      if("single" %in% class(params)){
        if(supress == FALSE){
          negative_control <- out[c('value_raw', 'investment', 'profit', 'rentability')] %>%
            unlist() %>%
            min()
          if(negative_control < 0){
            out[['value']] <- "Por favor, revise os valores."
          }else{
            out[['value']] <- ""
          }
        }
      }
      return(out)
    }
  }
  
  # Select Params: create a list to simulator ----
  selectParams_generic <- function(estimate_type, var_type){
    message("Entrou no selectParams Generic")
    # Get input value
    var <- input[[paste0('var_', estimate_type)]]/100
    years <- input[[paste0('years_', estimate_type)]]
    monthly_contribution <- input[[paste0('monthly_contribution_', estimate_type)]]
    total_money <- input[[paste0('total_money_', estimate_type)]]
    start <- input[[paste0('start_', estimate_type)]]
    message("Criou variaveis")
    
    # Force the main input be null
    for(null_var in var_type){
      if(input[[paste0(null_var, '_', estimate_type)]] == "Entrada"){
        start <- NULL
      }else if(input[[paste0(null_var, '_', estimate_type)]] == "Aporte mensal"){
        monthly_contribution <- NULL
      }else if(input[[paste0(null_var, '_', estimate_type)]] == "Montante final"){
        total_money <- NULL
      }else if(input[[paste0(null_var, '_', estimate_type)]] == "Anos"){
        years <- NULL
      }else if(input[[paste0(null_var, '_', estimate_type)]] == "Rentabilidade"){
        var <- NULL
      }
    }
    
    return(list(var = var,
                years = years,
                start = start,
                monthly_contribution = monthly_contribution,
                total_money = total_money))
  }
  
  # Select params to single
  selectParams_single <- reactive({
    params <- selectParams_generic(estimate_type = "single", var_type = "main") 
    return(params)
  })
  
  # Select params to graphs
  selectParams_graph <- reactive({
    params <- selectParams_generic(estimate_type = "graph", var_type = c("var1", "var2")) 
    return(params)
  })
  
  
  # Single simulate functions ----
  output$calculator_value <- renderValueBox({
    valueBox(
      simulator(params = selectParams_single())[['value']],
      subtitle = "",
      color = "light-blue"
    )})
  
  output$calculator_status <- renderText({ 
    simulator(params = selectParams_single(), supress = FALSE)[['value']]
  })
  output$investimentBox <- renderInfoBox({
    infoBox(
      "Investimento", 
      simulator(params = selectParams_single())[['investiment']],
      color = "blue", 
      fill = FALSE
    )
  })
  output$profitBox <- renderInfoBox({
    infoBox(
      "Lucro",
      simulator(params = selectParams_single())[['profit']],
      color = "olive", 
      fill = FALSE
    )
  })
  output$rentabilityBox <- renderInfoBox({
    infoBox(
      "Rentabilidade", 
      simulator(params = selectParams_single())[['rentability']],
      color = "navy", fill = F
    )
  })

  # Graph simulate function
  output$graph <- renderHighchart({
    info_plot <- simulator(params = selectParams_graph(), 
                           supress = TRUE, 
                           qtd_var = 2)[['value']]
    
    # info_plot$df %>%
    #   ggplot(aes(x = x, y = y)) +
    #   geom_line() +
    #   geom_point() +
    #   labs(x = info_plot$label_x,
    #        y = info_plot$label_y,
    #        title = info_plot$label_title)
    
    info_plot$df %>%
      hchart('line', hcaes(x = x, y = y), color = "blue") %>%
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T )) %>%
      hc_xAxis(title = list(text = info_plot$label_x)) %>%
      hc_yAxis(title = list(text = info_plot$label_y)) %>%
      hc_title(text = info_plot$label_title)
  })

}