library(shiny)

# Store modules in /R so that future versions will autoload them
source("R/data.R")
source("R/cobra.R")
source("R/league_links.R")


# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Greater Twin Cities Netrunner League Points"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            actionButton("update", "Update Standings", class = "btn-primary"),
            
            br(),
            br(),
            
            a("League Rules", href = league_rules),
            
            br(),
            br(),
            
            a("League Signup", href = league_signup),
            
            br(),
            br(),
            
            a("League Reporting", href = league_reporting),
            
                        
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            tableOutput("league_table")
            
        )
    )
)


# Define server
server <- function(input, output) {
    
    league_data <- eventReactive({input$action | input$update}, loadData())
    
    output$league_table <- renderTable(league_data())
    
}


# Run the application 
shinyApp(ui = ui, server = server)
