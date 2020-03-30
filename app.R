library(shiny)
library(dplyr)
library(purrr)

# Store modules in /R so that future versions will autoload them
source("R/data.R")
source("R/cobra.R")
source("R/league_links.R")


# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Greater Twin Cities Netrunner League"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            actionButton("update", "Update Standings", class = "btn-primary"),
            
            br(),
            br(),
            br(),
            
            selectInput("selected_pairings", "Pairing Week", 
                        choices = names(cobra_stats$rounds), 
                        selected = last(names(cobra_stats$rounds))),
            
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
            
            h4("League Standings"),
            tableOutput("league_table"),
            
            br(),
            br(),

            h4("Weekly Pairings"),            
            tableOutput("pairings")
            
        )
    )
)


# Define server
server <- function(input, output) {
    
    league_data <- eventReactive({input$action | input$update}, load_league_data())
    
    output$league_table <- renderTable(league_data())
    
    output$pairings <- renderTable(league_pairings(input$selected_pairings))
    
}


# Run the application 
shinyApp(ui = ui, server = server)
