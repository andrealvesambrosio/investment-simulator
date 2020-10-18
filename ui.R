# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Calculadora", 
             icon = icon("th"), 
             tabName = "single_simulate",
             badgeLabel = "new", 
             badgeColor = "green"),
    
    menuItem("Análise de gráficos", 
             tabName = "graph_simulate", 
             icon = icon("dashboard"))
    
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "graph_simulate",
            h2("Gráficos")
    ),
    
    tabItem(tabName = "single_simulate",
            h2("Calculadora"),
            
            selectInput(inputId = "main_single", 
                        label = "Valor a ser simulado", 
                        choices = c("Entrada", "Aporte mensal", "Montante final")),
            
            textOutput("calculator_status"),
            tags$head(tags$style("#calculator_status{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
                         )
            ),
            
            box(title = "Variáveis",
                solidHeader = TRUE,
                status = "primary",
                footer = valueBoxOutput("calculator_value",
                                        width = NULL),
                
                numericInput("var_single", 
                             "Rentabilidade ao mês (em %)", 
                             min = 0.1, 
                             max = 5, 
                             value = 0.5),
                
                numericInput("years_single", 
                             "Anos", 
                             min = 1,
                             max = 40, 
                             value = 10),
                
                conditionalPanel(condition = "input.main_single != 'Entrada'",
                                 numericInput("start_single", 
                                              "Entrada", 
                                              min = 0, 
                                              max = 100e3, 
                                              value = 10e3)
                ),
                conditionalPanel(condition = "input.main_single != 'Aporte mensal'",
                                 numericInput("monthly_contribution_single", 
                                              "Aporte mensal", 
                                              min = 0, 
                                              max = 10e3, 
                                              value = 1e3)
                ),
                 conditionalPanel(condition = "input.main_single != 'Montante final'",
                                  numericInput("total_money_single", 
                                               "Montante final", 
                                               min = 0, 
                                               max = 5e6, 
                                               value = 250e3)
                 )
            )
            
            #plotOutput('plot1')

            # A static valueBox
            

    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Simulador"),
  sidebar,
  body
)



# library(shiny)
# library(shinydashboard)
# runApp("C:/Users/55169/Desktop/Dev/investment-simulator")
