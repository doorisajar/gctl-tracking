library(googlesheets4)
library(lubridate)
library(magrittr)
library(dplyr)
library(janitor)

table <- "leagueTable"


loadData <- function() {

  sheet <- "https://docs.google.com/spreadsheets/d/1M5JmA4usZJ-5zk3hKrm8i90uSrGYEfpOd0w3EGKhk-g"
  
  # Read the data
  lg <- read_sheet(sheet)
  
  names(lg) <- c("report_timestamp",
                 "id",
                 "report_date",
                 "pairing_wins",
                 "new_ids",
                 "open_play_games",
                 "open_play_wins",
                 "opponents")

  lg %<>%
    mutate(pairing_wins = substr(pairing_wins, 1, 1) %>% as.integer,
           new_ids = substr(new_ids, 1, 1) %>% as.integer,
           report_week = week(report_date))
  
   points <-
     lg %>% 
     leagueStats() %>%
     ungroup() %>%
     group_by(id) %>%
     summarize(points = sum(points, na.rm = TRUE) %>% as.integer) %>%
     arrange(desc(points))
        
   names(points) <- c("Player", "League Points")
   
   points
  
}


leagueStats <- function(data) {
  
  data %<>%
    group_by(report_week, id) %>%
    mutate(points = leaguePoints(pairing_wins, new_ids, open_play_games, open_play_wins))
  
  data
  
}


leaguePoints <- function(pairing_wins, new_ids, open_play_games, open_play_wins) {
  
  pairing_wins <- sum(pairing_wins, na.rm = TRUE)
  new_ids <- sum(new_ids, na.rm = TRUE)
  open_play_games <- sum(open_play_games, na.rm = TRUE)
  open_play_wins <- sum(open_play_wins, na.rm = TRUE)
  
  pairing_points <- max(3 * pairing_wins, 0)
  new_id_points <- min(2 * new_ids, 4)
  open_game_points <- min(open_play_games, 4)
  open_win_points <- min(open_play_wins, 4)
  
  pairing_points + new_id_points + open_game_points + open_win_points
  
}
