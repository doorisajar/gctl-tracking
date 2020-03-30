library(jsonlite)

# Globals
cobra_link <- "http://cobr.ai/tournaments/1628.json"

cobra_stats <- fromJSON(cobra_link)
names(cobra_stats$rounds) <- paste("Week", 1:length(cobra_stats$rounds))


get_player_name <- function(p, stats = cobra_stats) {
  
  filter(stats$players, id == p) %>% 
    select(id, name)
  
}


league_pairings <- function(selected_pairings, stats = cobra_stats) {
  
  p1 <- map_dfr(stats$rounds[[selected_pairings]]$player1$id, get_player_name)
  
  p2 <- map_dfr(stats$rounds[[selected_pairings]]$player2$id, get_player_name)
  
  data.frame(Player1 = p1$name,
             Player2 = p2$name)
  
}
