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
            h2("Relações duplas")
    ),
    
    tabItem(tabName = "single_simulate",
            h2("Calculadora"),
            selectInput(inputId = "main", 
                        label = "Valor a ser simulado", 
                        choices = c("Entrada", "Aporte mensal", "Montante final")),
            sliderInput("var", "Rentabilidade ao mês (em %)", 0.1, 5, 0.5),
            sliderInput("years", "Anos", 1, 40, 10),
            conditionalPanel(
              condition = "input.main != 'Entrada'",
              sliderInput("start", "Entrada", 0, 100000, 10000)
            ),
            conditionalPanel(
              condition = "input.main != 'Aporte mensal'",
              sliderInput("monthly_contribution", "Aporte mensal", 0, 10000, 1000)
            ),
            conditionalPanel(
              condition = "input.main != 'Montante final'",
              sliderInput("total_money", "Montante final", 0, 40, 10)
            )
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Simulador"),
  sidebar,
  body
)


#runApp("C:/Users/55169/Desktop/Dev/investment-simulator")
