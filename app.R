library(shiny)

# Store modules in /R so that future versions will autoload them
source("R/data.R")


# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Greater Twin Cities Netrunner League Points"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("update", "Update", class = "btn-primary")
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
