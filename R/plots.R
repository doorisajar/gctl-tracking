library(ggplot2)
library(ggdark)
library(plotly)

# This function expects to receive the output of league_stats
daily_plot <- function(data, params) {

    data %<>%
        arrange(report_date) %>%
        group_by(id) %>%
        mutate(cumulative_points = cumsum(points)) %>%
        group_by(id, report_date) %>%
        summarize(cumulative_points = max(cumulative_points, na.rm = TRUE))

    players <- map_dfr(unique(data$id), zeroth_report, data, params)

    data <- bind_rows(players, data)

    static_plot <-
        ggplot(data, aes(x = report_date, y = cumulative_points, colour = id)) +
        geom_line(size = 1) +
        scale_colour_brewer(type = "seq", palette = "Spectral") +
        dark_theme_dark() +
        theme(legend.title = element_blank()) +
        xlab("") +
        ylab("Cumulative League Points")

    ggplotly(static_plot)

}


# a player's first appearance on the plot is either at 0 points on the start of the league week,
# or at whatever points they had from games reported on the league start date.
zeroth_report <- function(id, data, params) {

    id_reports <- filter(data, id == {{id}})

    zr <- list(id = NULL,
               report_date = NULL,
               cumulative_points = NULL)

    if (nrow(id_reports) > 0) {

        league_start <- ymd_hms(params$league_start)
        first_report <- min(id_reports$report_date)

        if (first_report > league_start) {

            # add 0 weeks for league week 1, 1 week for league week 2, etc.
            week_start <- league_start + dweeks(league_week(first_report, league_start) - 1)

            zr <- list(id = id,
                       report_date = week_start,
                       cumulative_points = 0)

        }

    }

    zr

}
