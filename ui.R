# Change decimal mark global R to ","
# Try to add big mark "." global to R
library(rjson)
input_rules <- fromJSON(file = "C:/Users/55169/Desktop/Dev/investment-simulator/check_input_value.json")

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
                #background = "navy",
                footer = valueBoxOutput("calculator_value",
                                        width = NULL),
                h6("Utilize '.' para casa decimal. Fora isso, sem caracteres especiais"),
                
                numericInput("var_single", 
                             "Rentabilidade ao mês (em %)", 
                             min = 100*input_rules[['var']][['min']], 
                             max = 100*input_rules[['var']][['max']], 
                             value = 100*input_rules[['var']][['start']],
                             step = 0.1),
                
                numericInput("years_single", 
                             "Anos", 
                             min = input_rules[['years']][['min']], 
                             max = input_rules[['years']][['max']], 
                             value = input_rules[['years']][['start']]),
                
                conditionalPanel(condition = "input.main_single != 'Entrada'",
                                 numericInput("start_single", 
                                              "Entrada", 
                                              min = input_rules[['start']][['min']], 
                                              max = input_rules[['start']][['max']], 
                                              value = input_rules[['start']][['start']])
                ),
                conditionalPanel(condition = "input.main_single != 'Aporte mensal'",
                                 numericInput("monthly_contribution_single", 
                                              "Aporte mensal", 
                                              min = input_rules[['monthly_contribution']][['min']], 
                                              max = input_rules[['monthly_contribution']][['max']], 
                                              value = input_rules[['monthly_contribution']][['start']])
                ),
                 conditionalPanel(condition = "input.main_single != 'Montante final'",
                                  numericInput("total_money_single", 
                                               "Montante final", 
                                               min = input_rules[['total_money']][['min']], 
                                               max = input_rules[['total_money']][['max']], 
                                               value = input_rules[['total_money']][['start']])
                 )
            ),
            
            infoBoxOutput("investimentBox", width = 4),
            infoBoxOutput("profitBox", width = 4),
            infoBoxOutput("rentabilityBox", width = 4)
            
            
            #plotOutput('plot1')

            # A static valueBox
            

    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Simulador"),
  sidebar,
  body,
  skin = "black"
)



# library(shiny)
# library(shinydashboard)
# runApp("C:/Users/55169/Desktop/Dev/investment-simulator")
