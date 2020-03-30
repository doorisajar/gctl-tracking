library(googlesheets4)
library(lubridate)
library(magrittr)
library(dplyr)
library(janitor)
library(jsonlite)


# We're only accessing public sheets
sheets_deauth()

# Globals
table <- "league_table"
league_start <- ymd_hms("2020-03-20 00:00:00")
sheet <- "https://docs.google.com/spreadsheets/d/1M5JmA4usZJ-5zk3hKrm8i90uSrGYEfpOd0w3EGKhk-g"


# Get the reporting table
load_league_data <- function() {
  
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
           report_week = league_week(report_date, league_start))
  
  lg
  
}


league_standings <- function(data) {
  
  points <-
    lg %>% 
    league_stats() %>%
    ungroup() %>%
    group_by(id) %>%
    summarize(points = sum(points, na.rm = TRUE) %>% as.integer) %>%
    arrange(desc(points)) %>%
    ungroup() %>%
    select(id, points)
  
  names(points) <- c("Player", "League Points")
  
  points
  
}


league_week <- function(report_date, league_start) {

  week <- 
    ((report_date - league_start) / dweeks(1)) %>% 
    floor()
  
  week + 1
  
}


league_stats <- function(data) {
  
  data %<>%
    rowwise() %>%
    mutate(points = league_points(pairing_wins, new_ids, open_play_games, open_play_wins))
  
  data
  
}


league_points <- function(pairing_wins, new_ids, open_play_games, open_play_wins) {
  
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
