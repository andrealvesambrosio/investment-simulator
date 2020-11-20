library(rjson)
library(highcharter)
input_rules <- rjson::fromJSON(file = "C:/Users/55169/Desktop/Dev/investment-simulator/check_input_value.json")

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
    # Graph simulate ----
    tabItem(tabName = "graph_simulate",
            fluidRow(column(width = 4,
                            #h3("Selecione as variáveis"),
                            fluidRow(column(width = 12, 
                                            h4("Selecione as variáveis"),
                                            selectInput(inputId = "var1_graph", 
                                                        label = "Primeira variável", 
                                                        choices = c("Aporte mensal", 
                                                                    "Entrada",
                                                                    "Montante final", 
                                                                    "Rentabilidade",
                                                                    "Anos")
                                                        ),
                            selectInput(inputId = "var2_graph", 
                                        label = "Segunda variável", 
                                        choices = c("Anos",
                                                    "Entrada", 
                                                    "Aporte mensal", 
                                                    "Montante final", 
                                                    "Rentabilidade")
                                        )
                            )
                            ),
                            fluidRow(column(width = 12,
                                            h4("Defina valores para as restantes"),
                                            conditionalPanel(condition = "input.var1_graph != 'Rentabilidade' &
                                                                         input.var2_graph != 'Rentabilidade'",
                                                             numericInput("var_graph",
                                                                          "Rentabilidade",
                                                                          min = 100*input_rules[['var']][['min']],
                                                                          max = 100*input_rules[['var']][['max']],
                                                                          value = 100*input_rules[['var']][['start']],
                                                                          step = 0.1)
                                                             ),
                                            conditionalPanel(condition = "input.var1_graph != 'Entrada' &
                                                                         input.var2_graph != 'Entrada'",
                                             numericInput("start_graph",
                                                          "Entrada",
                                                          min = input_rules[['start']][['min']],
                                                          max = input_rules[['start']][['max']],
                                                          value = input_rules[['start']][['start']])
                                             ),
                                            conditionalPanel(condition = "input.var1_graph != 'Anos' &
                                                                         input.var2_graph != 'Anos'",
                                                             numericInput("years_graph",
                                                                          "Anos",
                                                                          min = input_rules[['years']][['min']],
                                                                          max = input_rules[['years']][['max']],
                                                                          value = input_rules[['years']][['start']])
                                                             ),
                                            conditionalPanel(condition = "input.var1_graph != 'Aporte mensal' &
                                                                         input.var2_graph != 'Aporte mensal'",
                                                             numericInput("monthly_contribution_graph",
                                                                          "Aporte mensal",
                                                                           min = input_rules[['monthly_contribution']][['min']],
                                                                           max = input_rules[['monthly_contribution']][['max']],
                                                                           value = input_rules[['monthly_contribution']][['start']])
                                                             ),
                                            conditionalPanel(condition = "input.var1_graph != 'Montante final' &
                                                                         input.var2_graph != 'Montante final'",
                                                            numericInput("total_money_graph",
                                                                        "Montante final",
                                                                         min = input_rules[['total_money']][['min']],
                                                                         max = input_rules[['total_money']][['max']],
                                                                         value = input_rules[['total_money']][['start']])
                                            )
                            )
                            )
            ), h1(""),
            column(width = 8,
                      offset = 0,
                      fluidRow(column(width = 12,
                                      h3(""),
                                      highchartOutput('graph')
                                      )
                      )
            )
            )
    ),
                                    
# Single simulate ----
    tabItem(tabName = "single_simulate",
            h2("Calculadora"),
            
            selectInput(inputId = "main_single", 
                        label = "Valor a ser simulado", 
                        choices = c("Entrada", "Aporte mensal", "Montante final", "Anos")),
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
                h6("Utilize '.' para casa decimal. Fora isso, sem pontuações."),
                
                numericInput("var_single", 
                             "Rentabilidade ao mês (em %)", 
                             min = 100*input_rules[['var']][['min']], 
                             max = 100*input_rules[['var']][['max']], 
                             value = 100*input_rules[['var']][['start']],
                             step = 0.1),
                
                
                conditionalPanel(condition = "input.main_single != 'Anos'",
                                 numericInput("years_single", 
                                              "Anos", 
                                              min = input_rules[['years']][['min']], 
                                              max = input_rules[['years']][['max']], 
                                              value = input_rules[['years']][['start']])
                ),
                

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
            
            
            

            # A static valueBox
    )
  )
)

# Dashboard ----
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
