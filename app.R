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
    
    league_data <- eventReactive({input$action | input$update}, loadData())
    
    output$league_table <- renderTable(league_data())
    
    # TODO put this in a different column or something once it's working, or maybe have tabs to
    # choose from displaying standings, pairings, or plots
    output$pairings <- renderTable({
        
        get_player_name <- function(p) {
            
            filter(cobra_stats$players, id == p) %>% 
            select(id, name)
            
        }

        p1 <- map_dfr(cobra_stats$rounds[[input$selected_pairings]]$player1$id, get_player_name)
        
        p2 <- map_dfr(cobra_stats$rounds[[input$selected_pairings]]$player2$id, get_player_name)
 
        data.frame(Player1 = p1$name,
                   Player2 = p2$name)
                
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
