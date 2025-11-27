library(shiny) # Imports shiny library
library(shinydashboard) # Imports shiny dashboard
library(bslib)
source("optimal.R") # Since we will use the optimal.R, imports also
source("dataset.R") # Our data frame of original data.


# Define UI for application
ui = dashboardPage( # Uses a dashboardPage
  dashboardHeader(
    title = span("Pollution Reduction", style = "font-size: 20px;"), # Changes title and styles accordingly
    titleWidth = 280 # Changes titleWidth
  ),
  dashboardSidebar(
    width=280,
    sidebarMenu( # Sidebar, has a tab for about and help/tutorial, as well as the main optimization
      id = "tabs",
      menuItem("Optimization", tabName = "optimal"),
      menuItem("About", tabName = "about"),
      menuItem("Help/ Tutorial", tabName = "help")
    )
  ),
  dashboardBody(
    tags$head( # CSS, this is mostly for the table and card boxes.
      tags$style(HTML("
        body{
          font-family: 'Verdana';
        }
        
        .box {
          padding: 13px;
        }
        
        .table-container {
          margin: 5px;
          padding: 5px;
          overflow-x: auto;
        }
        table {
          margin-left: auto;
          margin-right: auto;
          padding: 20px;
          width: 80%;
        }
        table th, table td {
          padding: 10px;
          border: 2.5px solid #ddd;
        }
      "))
    ),
    tabItems( # This part of the code is for the different tabs
      tabItem(
        tabName = "optimal",
        
        fluidRow(
          column(
            width = 3,
            box(
              title = "Selection Pane",
              solidHeader = TRUE,
              width = 12,
              checkboxGroupInput(
                inputId = "select_cb",
                label = "Available Projects:",
                choices = reductionsPerUnit[[1]][1:30],
                selected = NULL
              ),
              actionButton("solve", "Solve Optimal", class = "btn-primary"),
              actionButton("select_all", "Select All", class = "btn-success", style = "margin-left:6px;"),
              actionButton("clear", "Clear", class = "btn-warning", style = "margin-left: 6px;")
            )
          ),
          
          column( # For the different cards, first is the final table, then initial tableau, then lastly is the iterations
            width = 9,
            box(
              title = "Optimal Projects",
              width = 12,
              solidHeader = TRUE,
              status = "info",
              uiOutput("total_cost"),
              div(class = "table-container", style = "margin-top: 12px;",
                  tableOutput("optimal_projs"))
            ),
            fluidRow(
              box(
                title = "Show Initial Tableau",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                status = "info",
                uiOutput("initial_tableau")
              )
            ),
            fluidRow(
              box(
                title = "Iterations",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                status = "info",
                uiOutput("iteration_log")
              )
            )
          )
        ),
      ),
      
      
      tabItem( # Tab for help / tutorial
        tabName = "help",
        fluidRow(
          box(title= "Help / Tutorial", width = 12, solidHeader = TRUE,
              p("1. Select projects on the left."),
              p("2. Click the Solve Optimal button to run the optimization."),
              p("3. Results appear on the right. Iterations will appear on the panel below"))
        )
      ),
      
      tabItem(
        tabName = "about",
        fluidRow(
          box(title = "About this app", width = 12, solidHeader = TRUE,
              p("This app is a City Pollution Reduction Plan app that computes how many units should be deployed of a speccific mitigation project, given that there are pollutants targeted to minimize."),
              br(),
              p("This app was made by Mari Gabriel D. De Leon | 2024-06258 | As final submission in CMSC 150 Project, Lab Section B5L."))
        )
      )
    )
  )
)

# Define server 
server = function(input, output) {
  selected_indices = reactiveValues(choices=c())
  
  observeEvent(input$clear, { # Updates functionality for clear button
    updateCheckboxGroupInput(
      inputId = "select_cb", 
      selected=FALSE
      )
  })
  
  observeEvent(input$select_all, { # Updates functionality for selectAll button
    updateCheckboxGroupInput(
      inputId = "select_cb", 
      selected = reductionsPerUnit[[1]][1:30]
      )
  })
  
  observeEvent(input$solve, { # Updates functonality for solve button
    req(input$select_cb)
    sel = match(input$select_cb, reductionsPerUnit[[1]][1:30])
    selected_indices$choices = sel
  })
  
  output$optimal_projs = renderTable({ # For rendering the final table with 
    req(selected_indices$choices) 
    minT = buildTableau(selected_indices$choices)
    result = getTable(minT)
    
    final_table = result$final_table
    output$total_cost = renderUI({
      if(is.na(result$Z)){
        div(
          span("Your selection is "),
          span("infeasible", style = "color:red; font-weight:700; margin-left:4px;"),
          br(),
          span("Number of iterations before ending: "),
          span(length(result$tableau_history), style = "color:red; font-weight:700; margin-left:4px;"),
        )
      } else {
        div(
          span("Your selection is "),
          span("feasible", style = "color:darkgreen; font-weight: 700; margin-left: 4px;"),
          br(),
          span("The cost of this optimal mitigation project is "),
          span(paste0('$',signif(result$Z,8)), style = "color:darkgreen; font-weight: 700; margin-left: 4px;")
        )
      }
    })
    final_table  # this will be displayed by renderTable
  }, width = "100%")
  
  output$initial_tableau = renderUI({ 
    req(selected_indices$choices)
    minT = buildTableau(selected_indices$choices)
    
    fluidRow(
      card(
        card_header("Initial Tableau"),
        tableOutput(outputId = "initialTab")
      )
    )
  })
  
  observe({
    req(selected_indices$choices)
    minT = buildTableau(selected_indices$choices)
    
    output[["initialTab"]] = renderTable({
      minT$tableau
    })
  })
  
  
  output$iteration_log = renderUI({
    req(selected_indices$choices)
    minT = buildTableau(selected_indices$choices)
    result = getTable(minT)
    
    n = length(result$tableau_history) # Sets n as length of tableau_history, or how many iterations
    fluidRow(
      lapply(1:n, function(i){
        card(
          card_header(
            if(i==1){
              "First Iteration"
            } else if(i==n){
              "Final Tableau"
            } else {
              paste("Iteration", i)
            }
          ),
          tableOutput(outputId = paste("tableau",i,sep="")),
          tableOutput(outputId = paste("basicSolution",i,sep=""))
        )
      })
    )
  })
  
  observe({
    req(selected_indices$choices)
    
    minT = buildTableau(selected_indices$choices)
    result = getTable(minT)
    n = length(result$tableau_history)
    
    lapply(1:n, function(i){
      output[[paste0("tableau", i)]] = renderTable({
        result$tableau_history[[i]]
      })
      
      output[[paste0("basicSolution", i)]] = renderTable({
        t(result$basic_solution_history[[i]])
      })
    })
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
