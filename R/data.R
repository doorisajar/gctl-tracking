library(googlesheets4)

# We're only accessing public sheets
sheets_deauth()

# Globals
table <- "league_table"


# Get the reporting table
load_league_data <- function(params) {

    # Read the data
    lg <- read_sheet(params$league_stats)

    names(lg) <- params$report_cols

    lg %<>%
        mutate(
            pairing_wins = substr(pairing_wins, 1, 1) %>% as.integer,
            new_ids = substr(new_ids, 1, 1) %>% as.integer,
            report_week = league_week(report_date, ymd_hms(params$league_start))
        )

    lg

}


league_standings <- function(data, params) {

    points <-
        data %>%
        league_stats(params) %>%
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


league_stats <- function(data, params) {

    point_cols <- keep(params$report_cols, ~ .x %in% names(params$report_points))

    points <- map2_dfc(point_cols, params$report_points, league_points, data = data)

    data$points <- rowSums(points, na.rm = TRUE)

    data

}


league_points <- function(col, values, data) {

    val <- data[[col]]

    val <- pmax(val, values[[2]], na.rm = TRUE)

    val <- pmin(val, values[[3]], na.rm = TRUE)

    point_val <- val * values[[1]]

    point_val

}


bounty_targets <- function(data, week, params) {

    # TODO return a table that can be displayed on the pairings tab adjacent to the pairings.

    standings <- league_standings(data, params)

    standings %<>%
        filter(league_week < week)

    if (!is.null(params$bounty_threshold) & is.numeric(params$bounty_threshold))
        standings <- standings[1:params$bounty_threshold, ]

    # return however many player names are above the bounty threshold as a table for display
    standings %>%
        select(Player)

}
