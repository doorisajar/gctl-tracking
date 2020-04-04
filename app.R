library(shiny)
library(shinythemes)

# General utility packages
library(dplyr)
library(purrr)
library(lubridate)
library(magrittr)
library(janitor)

# Store modules in /R so that future versions will autoload them
source("R/data.R")
source("R/cobra.R")
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

            a("League Rules", href = params$league_rules),

            br(),

            a("League Signup", href = params$league_signup),

            br(),

            a("League Reporting", href = params$league_reporting),

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

                tabPanel("Timeseries", plotlyOutput("cumulative_plot"))

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
    output$cumulative_plot <- renderPlotly(daily_plot(league_stats(league_data)))

}


# Run the application
shinyApp(ui = ui, server = server)
