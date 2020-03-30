library(ggplot2)
library(ggdark)

# This function expects to receive the output of league_stats
daily_plot <- function(data) {

    data %<>%
        arrange(report_date) %>%
        group_by(id) %>%
        mutate(cumulative_points = cumsum(points)) %>%
        group_by(id, report_date) %>%
        summarize(cumulative_points = max(cumulative_points, na.rm = TRUE))

    # TODO make the legend bigger
    static_plot <-
        ggplot(data, aes(x = report_date, y = cumulative_points, colour = id)) +
        geom_line(size = 1) +
        scale_colour_brewer(type = "seq", palette = "Spectral") +
        dark_theme_dark() +
        theme(legend.title = element_blank()) +
        xlab("") +
        ylab("Cumulative League Points")

    # TODO this fails to render in the app when made interactive with plotly
    ggplotly(static_plot)

}
