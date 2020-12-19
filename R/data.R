library(googlesheets4)

# We're only accessing public sheets
gs4_deauth()

# Globals
table <- "league_table"


# get the cobra data
get_rounds <- function(cobra_stats) {

    if (length(cobra_stats$rounds) == 0)
        r <- NULL
    else
        r <- paste("Week", 1:length(cobra_stats$rounds))

    r

}


get_cobra_stats <- function(url) {

    cobra_stats <- fromJSON(url)
    names(cobra_stats$rounds) <- get_rounds(cobra_stats)

    cobra_stats

}


# Get the reporting table
load_league_data <- function(params) {

    # Read the data
    lg <- read_sheet(params$league_stats)

    # Remove rows that are entirely NA
    lg <- lg[rowSums(is.na(lg)) != ncol(lg), ]

    names(lg) <- params$report_cols

    lg %<>%
        mutate(
            pairing_wins = substr(pairing_wins, 1, 1) %>% as.integer,
            new_ids = substr(new_ids, 1, 1) %>% as.integer,
            report_week = league_week(report_date, ymd_hms(params$league_start))
        )

    lg

}
