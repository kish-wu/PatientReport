#' @title plot_time_series_by_date
#' @description Plot a line graph of the time series acceleration by date
#' @param IMP The IMP object from GGIR Part 2
#' @param summary_data A data frame with date and min/day of sleep, inactivity, light, and moderate-to-vigorous physical activity
#' @return A ggplot2 object
#' @details Plot a line graph of the time series acceleration by date
#' @seealso
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{mutate}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_path}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{facet_wrap}}, \code{\link[ggplot2]{vars}}, \code{\link[ggplot2]{ggtheme}}, \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{element}}
#' @rdname plot_time_series_by_date
#' @export
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggplot geom_line aes facet_wrap vars theme_classic theme element_blank element_text
#' @importFrom scales date_format

plot_time_series_by_date <- function(IMP, summary_data) {
  data <- IMP$metashort

  data$timestamp <-
    as.POSIXct(
      data$timestamp,
      format = "%Y-%m-%dT%H:%M:%S%z",
      tz = "UTC"
    )

  data2plot <-
    data |>
    dplyr::group_by(timestamp = format(timestamp, "%Y-%m-%d %H:%M")) |>
    dplyr::summarise(ENMO = mean(ENMO)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      category = cut(
        ENMO * 1000,
        breaks = c(-Inf, 44.8, 100.6, 428.8, Inf),
        labels = c("Inactivity", "Light", "Moderate", "Vigorous")
      )
    ) |>
    dplyr::mutate(
      timestamp = as.POSIXct(timestamp, "%Y-%m-%d %H:%M", tz = "UTC")
    ) |>
    dplyr::arrange(timestamp)

  intcat <- unique(data2plot$category)
  n_colors <- length(intcat)
  colors <- c("#a9a9a9", "#0051ba", "#e8000d", "#ffc82d")
  date_labels <- format(unique(as.Date(data$timestamp)), "%A, %B %d, %Y")

  summary_data$date <- as.Date(summary_data$date, format = "%m/%d/%Y")
  summary_data$label <-
    paste(
      sprintf(
        "Sleep: %.1f; Inactivity: %.1f;",
        round(summary_data$dur_spt_min / 60, 1),
        round(summary_data$dur_day_total_IN_min / 60, 1)
      ),
      sprintf(
        "Light: %i; MVPA: %i",
        summary_data$dur_day_total_LIG_min,
        summary_data$dur_day_total_MVPA_min
      )
    )

  maxENMO <- max(data2plot$ENMO, na.rm = TRUE)

  lengthUniqueDates <- length(unique(as.Date(data2plot$timestamp)))

  numberRowsFacets <- dplyr::case_when(
    lengthUniqueDates == 7 ~ 4,
    lengthUniqueDates == 6 ~ 3,
    lengthUniqueDates == 5 ~ 3,
    TRUE ~ 2
  )

  plot <- data2plot |>
    dplyr::mutate(date = as.Date(timestamp), .before = 1) |>
    dplyr::left_join(y = summary_data, by = "date") |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(
      x = timestamp,
      y = ENMO,
      color = category,
      group = 1
    )) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = as.POSIXct(paste(date, "12:00:00"), tz = "UTC"),
        y = maxENMO + 0.2,
        label = label,
        group = 1,
      ),
      size = 5,
      color = "#1C3B6A",
      fontface = "bold"
    ) +
    ggplot2::expand_limits(y = c(0, maxENMO + 0.3)) +
    ggplot2::facet_wrap(
      ggplot2::vars(
        factor(
          format(as.Date(timestamp), "%A, %B %d, %Y"),
          levels = date_labels
        )
      ),
      nrow = numberRowsFacets,
      scales = "free_x"
    ) +
    ggplot2::scale_x_datetime(
      labels = scales::date_format("%k"),
      breaks = "2 hour",
      expand = c(0, 0)
    ) +
    ggplot2::scale_color_manual(
      values = colors[1:n_colors],
      labels = c("", as.character(intcat[2:n_colors]))
    ) +
    ggplot2::xlab("Hour of the Day") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 10),
        face = "bold",
        size = 18
      ),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 18,
        margin = ggplot2::margin(b = 15)
      ),
      legend.position = c(0.8, -0.1), # deprecated in ggplot2 3.5.0; use legend.position.inside
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12),
      plot.margin = ggplot2::margin(b = 20),
      panel.spacing = ggplot2::unit(2, "lines")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(color = c(NA, colors[2:n_colors]), linewidth = 1.5)
      )
    )

  return(plot)
}
