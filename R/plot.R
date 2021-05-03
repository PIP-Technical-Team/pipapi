custom_plot <- function(lkups) {

  p <- ggplot2::ggplot(lkups$svy_lkup, ggplot2::aes(x = reporting_year,
                                                    y = survey_mean_ppp,
                                                    color = pcn_region_code,
                                                    group = pcn_region_code)) +
    ggplot2::geom_smooth()

  plotly::ggplotly(p)
}
