library(jsonlite)

# Globals
cobra_link <- "http://cobr.ai/tournaments/1628.json"

cobra_stats <- fromJSON(cobra_link)
names(cobra_stats$rounds) <- paste("Week", 1:length(cobra_stats$rounds))

# TODO make a sidebar dropdown that lists the weeks to date and then displays that week's pairings
# and unpaired players. then this function can take the week from the UI and return a table of 
# pairings to be displayed. 

league_pairings <- function(week) {
  
  
  
}
