# compute the cumulative league standings, accounting for weekly points caps. store each week's
# information (reports with associated points, and bounty targets) in a list.
league_tracker <- function(data, params) {

    data$league_week <- league_week(data$report_date, ymd_hms(params$league_start))

    weeks <- unique(data$league_week)

    weekly_stats <- list()

    # track bounty targets week to week to ensure bounty targets don't claim bounty win points.
    for (week in weeks) {

        week_data <- filter(data, league_week == week)

        if (week == 1) {

            targets <- tibble(`Bounty Target` = "None yet!",
                              `League Points at Week Start` = "NA")

        } else if (week > 1) {

            targets <- bounty_targets(weekly_stats[[paste("Week", week - 1)]]$scores, paste("Week", week - 1), params)

            week_data %<>%
                mutate(open_play_wins = ifelse(id %in% targets, 0, open_play_wins))

        } else {

            stop("League weeks can only be positive integers.")

        }

        weekly_stats[[paste("Week", week)]] <- list(scores = league_points(week_data, params), targets = targets)

    }

    weekly_stats

}


league_standings <- function(league) {

    points <- map_dfr(league, ~ .x$scores)

    league_point_totals(points)

}


league_point_totals <- function(data) {

    points <-
        data %>%
        ungroup() %>%
        group_by(id) %>%
        summarize(points = sum(points, na.rm = TRUE) %>% as.integer) %>%
        arrange(desc(points)) %>%
        ungroup() %>%
        select(id, points)

    names(points) <- c("Player", "League Points")

    points

}


# compute "weeks elapsed since league start + 1"
league_week <- function(report_date, league_start) {

    # this could be done much more simply with a factor and breaks
    week <-
        ifelse(
            report_date == league_start,
            1,
            ((report_date - league_start) / dweeks(1)) %>%
                ceiling()
        )

    week

}


league_points <- function(data, params) {

    point_cols <- keep(params$report_cols, ~ .x %in% names(params$report_points))
    names(point_cols) <- paste0(point_cols, "_points")

    points <- map_dfc(point_cols, report_points, data, params$report_points)

    points <- bind_cols(data, points)

    capped_points <-
        points %>%
        group_by(id, report_week) %>%
        summarize(across(matches("_points"), threshold_points, params$report_points))

    total_points <-
        capped_points %>%
        ungroup %>%
        select(matches("_points")) %>%
        rowSums()

    # ensure the rows align before essentially column binding the point totals
    data %<>%
        arrange(id, report_week) %>%
        mutate(points = total_points)

    data

}


# compute the point value of an individual element of an individual report, without other knowledge
report_points <- function(category, data, point_values) {

    counts <- data[[category]]

    values <- point_values[[category]]

    counts <- pmax(counts, values$min, na.rm = TRUE)

    counts <- pmin(counts, values$max, na.rm = TRUE)

    points <- counts * values$points

    points

}


# for a given player & week, their points are the pmin of their individual report points and the
# weekly league thresholds for each category.
threshold_points <- function(player_points, point_limits) {

    limits <- point_limits[[str_remove(cur_column(), "_points")]]

    player_points[is.na(player_points)] <- 0

    cap <- limits$points * limits$max

    hit_cap <- cumsum(player_points) > cap

    capped_points <- ifelse(hit_cap, 0, player_points)

    # the first report where a player hits a cap may still get partial points
    capped_points[which(hit_cap)[1]] <- cap - player_points[which(hit_cap)[1]]

    capped_points

}


bounty_targets <- function(data, week, params) {

    standings <- league_point_totals(data)

    if (!is.null(params$bounty_threshold) & is.numeric(params$bounty_threshold)) {

        bounty_cutoff <- standings[params$bounty_threshold, ]$`League Points`
        standings <- filter(standings, `League Points` >= bounty_cutoff)

    } else {

        standings <- standings[0, ]

    }

    # return however many player names are above the bounty threshold as a table for display
    targets <-
        standings %>%
        select(`Bounty Target` = Player,
               `League Points at Week Start` = `League Points`)

    targets

}
