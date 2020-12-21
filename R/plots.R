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

    players <- map_dfr(unique(data$id), function(id) list(id = id,
                                                          report_date = ymd_hms(params$league_start),
                                                          cumulative_points = 0))

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
