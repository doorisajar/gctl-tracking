library(shiny)
library(shinythemes)

# These are used in multiple source files
library(dplyr)
library(purrr)

# Store modules in /R so that future versions will autoload them
source("R/data.R")
source("R/cobra.R")
source("R/league_links.R")
source("R/plots.R")


# Define UI
ui <- fluidPage(theme = shinytheme("darkly"),

    # Application title
    titlePanel("Greater Twin Cities Netrunner League"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 3,

            strong("League Links"),

            br(),

            a("League Rules", href = league_rules),

            br(),

            a("League Signup", href = league_signup),

            br(),

            a("League Reporting", href = league_reporting),

            conditionalPanel(condition = "input.selected_tab == 'Pairings'",
                             br(),
                             br(),
                             selectInput("selected_pairings", "Pairing Week",
                                         choices = names(cobra_stats$rounds),
                                         selected = last(names(cobra_stats$rounds)))),

        ),

        # Show a plot of the generated distribution
        mainPanel(

            tabsetPanel(type = "tabs", id = "selected_tab",

                tabPanel("Standings", tableOutput("league_table")),

                tabPanel("Pairings", tableOutput("pairings")),

                tabPanel("Timeseries", plotOutput("cumulative_plot"))

            )

        )
    )
)


# Define server
server <- function(input, output) {

    league_data <- load_league_data()

    # Cumulative league standings
    output$league_table <- renderTable(league_standings(league_data))

    # Pairings for a user-selected week (default latest week)
    output$pairings <- renderTable(league_pairings(input$selected_pairings))

    # Timeseries plot of cumulative points for each player
    output$cumulative_plot <- renderPlot(daily_plot(league_stats(league_data)))

}


# Run the application
shinyApp(ui = ui, server = server)
