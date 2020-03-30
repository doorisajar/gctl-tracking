library(ggplot2)
library(plotly)

# This function expects to receive the output of league_stats
daily_plot <- function(data) {

    data %<>%
        arrange(report_date) %>%
        group_by(report_date, id) %>%
        mutate(cumulative_points = cumsum(points)) %>%
        summarize(cumulative_points = sum(cumulative_points, na.rm = TRUE))

    p <-
        ggplot(tmp, aes(x = report_date, y = cumulative_points, colour = id)) +
        geom_line()

    ggplotly(p)

}
