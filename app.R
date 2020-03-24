library(shiny)

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Greater Twin Cities Netrunner League Points"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("leagueTable")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$leagueTable <- renderTable({
        
        loadData()
        
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
