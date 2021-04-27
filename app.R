library(shiny)
library(shinythemes)

# General utility packages
library(dplyr)
library(purrr)
library(lubridate)
library(magrittr)
library(janitor)
library(stringr)

params_path <- "gctl_config.json"

# Load manually instead of autoloading so we get the right order
source("R/params.R")
source("R/league.R")
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

                                    tabPanel("Pairings", fluidRow(tableOutput("pairings"),
                                                                  tableOutput("bounty_targets"))),

                                    tabPanel("Timeseries", plotlyOutput("cumulative_plot"))

                        )

                    )

                )

)


# Define server
server <- function(input, output) {

    params <- read_params(params_path)

    league_data <- load_league_data(params)

    league <- league_tracker(league_data, params)

    if (nrow(league_data) == 0) {

        # Cumulative league standings
        output$league_table <- renderTable(tibble(Player = "None yet!",
                                                  `League Points` = "NA"))

        # Pairings for a user-selected week (default latest week)
        output$pairings <- renderTable(league_pairings(input$selected_pairings))

    } else {

        # Cumulative league standings
        output$league_table <- renderTable(league_standings(league))

        # Pairings for a user-selected week (default latest week)
        output$pairings <- renderTable(league_pairings(input$selected_pairings))

        output$bounty_targets <- renderTable(league[[input$selected_pairings]]$targets)

        # Timeseries plot of cumulative points for each player
        output$cumulative_plot <- renderPlotly(daily_plot(map_dfr(league, ~ .x$scores), params))

    }

}


# Run the application
shinyApp(ui = ui, server = server)
