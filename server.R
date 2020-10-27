# Libraries ----
library(dplyr)
library(ggplot2)
library(rjson)

# Source ----
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/theme_swd.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/control_graph.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/make_simulation.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/check_inputs.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/get_formula.R")
input_rules <- fromJSON(file = "C:/Users/55169/Desktop/Dev/investment-simulator/check_input_value.json")

# Server ----
function(input, output, session) {
  
  # Simulator
  simulator <- function(params, supress = TRUE){
    message("Entrou no simulator")
    status_inputs <- check_inputs(params, input_rules, supress = supress)
    message("Rodou o check_inputs")
    if(status_inputs[['status']] != "OK"){
      message("check_inputs deu erro\n")
      return(status_inputs)
    }else{
      message("check_inputs deu certo\n")
      info_to_simulate <- get_formula(params)
      class(params) <- c(info_to_simulate[['simulate_type']],
                         info_to_simulate[['axis_x']])
      
      out <- make_simulation(params = params, 
                            info = info_to_simulate)
      if(supress == FALSE){
        negative_control <- out[c('value_raw', 'investment', 'profit', 'rentability')] %>%
          unlist() %>%
          min()
        message("aopa")
        message(negative_control)
        if(negative_control < 0){
          out[['value']] <- "Por favor, revise os valores."
        }else{
          out[['value']] <- ""
        }
      }
      return(out)
    }
  }
  
  # Combine the selected variables into a new data frame
  selectedParams <- reactive({
    message("Entrou no selectedParams")
    # Get input value
    var = input$var_single/100
    years = input$years_single
    monthly_contribution = input$monthly_contribution_single
    total_money = input$total_money_single
    start = input$start_single
    message("Criou variaveis")
    
    # Force the main input be null
    if(input$main_single == "Entrada"){
      start <- NULL
    }else if(input$main_single == "Aporte mensal"){
      monthly_contribution <- NULL
    }else if(input$main_single == "Montante final"){
      total_money <- NULL
    }else if(input$main_single == "Anos"){
      years <- NULL
    }
    message("Saindo do selectedParams\n")
    
   return(list(var = var, 
               years = years, 
               monthly_contribution = monthly_contribution, 
               total_money = total_money, 
               start = start))
  })
  
  output$calculator_value <- renderValueBox({
    valueBox(
      simulator(params = selectedParams())[['value']],
      subtitle = "",
      color = "light-blue"
    )})
  
  output$calculator_status <- renderText({ 
    simulator(params = selectedParams(), supress = FALSE)[['value']]
  })
  output$investimentBox <- renderInfoBox({
    infoBox(
      "Investimento", 
      simulator(params = selectedParams())[['investiment']],
      color = "blue", 
      fill = FALSE
    )
  })
  output$profitBox <- renderInfoBox({
    infoBox(
      "Lucro",
      simulator(params = selectedParams())[['profit']],
      color = "olive", 
      fill = FALSE
    )
  })
  output$rentabilityBox <- renderInfoBox({
    infoBox(
      "Rentabilidade", 
      simulator(params = selectedParams())[['rentability']],
      color = "navy", fill = F
    )
  })

  output$plot1 <- renderPlot({
    data.frame(x = c(1:10),
               y = c(21:30)) %>%
      ggplot2::ggplot(aes(x = x, y = y)) +
      geom_point() #+
      #theme_swd()
  })
}