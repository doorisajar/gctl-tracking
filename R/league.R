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

    if (is_empty(report_date)) {

        week <- 0

    } else{

        week <-
            ((report_date - league_start) / dweeks(1)) %>%
            floor()
    }

    week + 1

}


league_stats <- function(data, params) {

    point_cols <- keep(params$report_cols, ~ .x %in% names(params$report_points))

    # this applies `...1` style column names, but it doesn't really matter
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

    week_num <- as.numeric(str_split(week, " ", simplify = TRUE)[2])

    # not a fan of early return, but it isn't terrible here
    if (week_num == 1)
        return(tibble(`Bounty Target` = "None yet!",
                      `League Points at Week Start` = "0"))

    data %<>%
        filter(report_week < week_num)

    standings <- league_standings(data, params)

    if (!is.null(params$bounty_threshold) & is.numeric(params$bounty_threshold))
        standings <- standings[1:params$bounty_threshold, ]
    else
        standings <- standings[0, ]

    # return however many player names are above the bounty threshold as a table for display
    targets <-
        standings %>%
        select(`Bounty Target` = Player,
               `League Points at Week Start` = `League Points`)

    targets[1:params$bounty_threshold, ]

}
