library(jsonlite)

# Globals
cobra_stats <- fromJSON(params$cobra_link)
names(cobra_stats$rounds) <- paste("Week", 1:length(cobra_stats$rounds))


get_player_name <- function(p, stats = cobra_stats) {

    name <-
        filter(stats$players, id == p) %>%
        pull(name)

    ifelse(is.na(p), NA, name)

}


league_pairings <- function(selected_pairings, stats = cobra_stats) {

    p1 <- map_chr(stats$rounds[[selected_pairings]]$player1$id, get_player_name)

    p2 <- map_chr(stats$rounds[[selected_pairings]]$player2$id, get_player_name)

    tibble(Player1 = p1,
           Player2 = p2)

}
