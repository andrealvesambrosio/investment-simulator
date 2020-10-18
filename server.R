# Libraries ----
library(dplyr)
library(ggplot2)

# Source ----
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/theme_swd.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/control_graph.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/make_simulation.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/check_inputs.R")
source("C:/Users/55169/Desktop/Dev/investment-simulator/R/get_formula.R")

# Server ----
function(input, output, session) {
  
  # Simulator
  simulator <- function(params){
    status_inputs <- check_inputs(params)
    if(status_inputs[['status']] != "OK"){
      return(status_inputs$value)
    }else{
      info_to_simulate <- get_formula(params)
      class(params) <- c(info_to_simulate[['simulate_type']],
                         info_to_simulate[['axis_x']])
      
      out = make_simulation(params = params, 
                            info = info_to_simulate)
    }
    return(out)
  }
  
  # Combine the selected variables into a new data frame
  selectedParams <- reactive({
    
    # Get input value
    var = input$var_single/100
    years = input$years_single
    monthly_contribution = input$monthly_contribution_single
    total_money = input$total_money_single
    start = input$start_single
    
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
    
   return(list(var = var, 
               years = years, 
               monthly_contribution = monthly_contribution, 
               total_money = total_money, 
               start = start))
  })
  
  output$calculator_value <- renderValueBox({
    valueBox(
      simulator(params = selectedParams())[['value']],
      "Progress", icon = icon("list"),
      color = "black"
    )})
  
  output$calculator_status <- renderText({ 
    simulator(params = selectedParams())[['status']]
  })

  
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}