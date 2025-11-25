#' @title generate_bmi_arch_plot
#' @description Generates an arch plot of the participant's body mass index and
#'     its associated risk level for cardiometabolic conditions.
#' @param wt Research participant's weight in pounds
#' @param ht Research participant's height in inches
#' @return An arch plot of the participants body mass index and its associated risk level.
#' @details Generates an arch plot of the participant's body mass index and
#'     its associated risk level for cardiometabolic conditions.
#' @seealso
#'  \code{\link[ggplot2]{geom_polygon}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{geom_label}}, \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_raster}}, \code{\link[ggplot2]{annotate}}, \code{\link[ggplot2]{scale_continuous}}, \code{\link[ggplot2]{coord_polar}}, \code{\link[ggplot2]{ggtheme}}
#' @rdname generate_bmi_arch_plot
#' @export
#' @importFrom ggplot2 geom_polygon aes geom_text ggplot geom_rect annotate scale_x_continuous scale_y_continuous coord_radial theme_void

generate_bmi_arch_plot <- function(wt, ht) {
  # Define internal BMI functions ----

  add_bmi_arrow <- function(bmi, curve = 0.75) {
    yvals <- c(-200, -200, -75, -200)
    list(
      ggplot2::geom_polygon(
        ggplot2::aes(x = c((bmi - curve), bmi, bmi, (bmi - curve)), y = yvals),
        fill = "grey80",
        alpha = 0.5,
        color = "black",
        linewidth = 0.75
      ),
      ggplot2::geom_polygon(
        ggplot2::aes(x = c(bmi, (bmi + curve), bmi, bmi), y = yvals),
        fill = "grey80",
        alpha = 0.5,
        color = "black",
        linewidth = 0.75
      )
    )
  }

  add_bmi_text <- function(categories, units) {
    list(
      ggplot2::geom_text(
        ggplot2::aes(
          x = c(15, 21.7, 27.5, 33),
          y = c(100, 100, 100, 100),
          label = categories
        ),
        fontface = "bold",
        size = 4.5
      ),
      ggplot2::geom_text(
        ggplot2::aes(
          x = c(15, 21.7, 27.5, 33),
          y = c(-30, -35, -30, -30),
          label = units
        )
      )
    )
  }

  # Create BMI Plot ----

  wt_kg <- wt / 2.205
  ht_m <- (ht * 2.54) / 100
  bmi <- wt_kg / ht_m^2

  if (bmi > 34) {
    bmi <- 34
  } # Set max bmi for radial plot

  ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = c(12, 18.5, 25, 30),
        xmax = c(18.5, 25, 30, 35),
        ymin = -200,
        ymax = 200
      ),
      fill = c("#ADD8E6", "#90EE90", "#FDEE8C", "#FF7F7F"),
      color = "black",
      linewidth = 1
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = 12, xmax = 35, ymin = -75, ymax = 25),
      fill = "grey80",
      alpha = .5,
      color = "black",
      linewidth = 0.75
    ) +
    ggplot2::annotate(
      "segment",
      x = c(12, 18.5, 25, 30),
      xend = c(12, 18.5, 25, 30),
      y = -200,
      yend = 200,
      linewidth = 1
    ) +
    add_bmi_arrow(bmi, curve = 0.75) +
    ggplot2::scale_x_continuous(breaks = seq(13, 34, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(
      breaks = seq(-200, 200, 100),
      expand = c(0, 0)
    ) +
    add_bmi_text(
      categories = c("Underweight", "Normal", "Overweight", "Obesity"),
      units = c(
        "< 18.5 kg/m\u00B2",
        "18.5-24.9 kg/m\u00B2",
        "25-29.9 kg/m\u00B2",
        "\u2265 30 kg/m\u00B2"
      )
    ) +
    ggplot2::coord_radial(
      start = -0.5 * pi,
      end = 0.5 * pi,
      inner.radius = 0.4,
      r.axis.inside = TRUE,
      rotate.angle = TRUE
    ) +
    ggplot2::theme_void()
}

#' @title generate_weight_bar_plot
#' @description Generates a bar plot of the participant's weight and its
#'     comparison to the weight at a healthy body mass index.
#' @param wt Research participant's weight in pounds
#' @param ht Research participant's height in inches
#' @return A bar plot of the participant's weight and its comparison to the weight at a healthy body mass index.
#' @details Generates a bar plot of the participant's weight and its
#'     comparison to the weight at a healthy body mass index.
#' @seealso
#'  \code{\link[ggplot2]{geom_segment}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{annotate}}, \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_raster}}, \code{\link[ggplot2]{geom_abline}}, \code{\link[ggplot2]{scale_continuous}}, \code{\link[ggplot2]{labs}}, \code{\link[ggplot2]{coord_fixed}}, \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{element}}, \code{\link[ggplot2]{reexports}}
#' @rdname generate_weight_bar_plot
#' @export
#' @importFrom ggplot2 geom_segment aes geom_point annotate ggplot geom_rect geom_hline geom_vline scale_x_continuous scale_y_continuous labs coord_fixed theme element_text margin element_blank element_line unit element_rect

generate_weight_bar_plot <- function(wt, ht) {
  # Define internal weight functions ----

  calculate_bmi_weight_ranges <- function(ht) {
    ht_m <- (ht * 2.54) / 100
    bmi_cutoffs <- c(10, 18.5, 24.9, 29.9, 60)
    weight_cutoffs <- bmi_cutoffs * (ht_m^2) * 2.205
    plot_max_weight <- weight_cutoffs[5] + 70

    wtrange <- seq(
      floor(weight_cutoffs[2] - 30),
      ceiling(plot_max_weight)
    )
    return(list(
      wt = wt,
      ht_m = ht_m,
      bmi_cutoffs = bmi_cutoffs,
      weight_cutoffs = weight_cutoffs,
      wtrange = wtrange,
      plot_max_weight = plot_max_weight
    ))
  }

  add_wt_line <- function(info, y.adjust.text = 0.4) {
    list(
      ggplot2::geom_segment(
        ggplot2::aes(
          x = min(info$wtrange),
          xend = info$wt,
          y = y.adjust.text,
          yend = y.adjust.text
        ),
        color = "black",
        linewidth = 1.5
      ),
      ggplot2::geom_point(
        ggplot2::aes(x = info$wt, y = y.adjust.text),
        fill = "black",
        size = 4
      )
    )
  }

  add_wt_text <- function(info, y.adjust.line = 0.4) {
    xvals <- sapply(1:4, function(i) {
      mean(c(
        c(min(info$wtrange), info$weight_cutoffs[2:4])[i],
        info$weight_cutoffs[2:5][i]
      ))
    })
    list(
      ggplot2::annotate(
        "text",
        x = xvals,
        y = rep(0.95, 4),
        label = c("Under", "Normal", "Over", "Obesity"),
        fontface = "bold",
        size = 4
      ),
      ggplot2::geom_label(
        ggplot2::aes(x = info$wt + 5, y = y.adjust.line),
        label = paste(info$wt, "lbs"),
        fill = "white",
        fontface = "bold",
        size = 4,
        hjust = 0
      )
    )
  }

  # Create weight plot ----
  info <- calculate_bmi_weight_ranges(ht)
  category_fills <- c("#ADD8E6", "#90EE90", "#FDEE8C", "#F08080")
  ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = min(info$wtrange),
        xmax = max(info$wtrange),
        ymin = 0,
        ymax = 1.1
      ),
      fill = "white"
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = c(min(info$wtrange), info$weight_cutoffs[2:4]),
        xmax = c(info$weight_cutoffs[2:4], max(info$wtrange)),
        ymin = c(.75, 0, .75, .75),
        ymax = c(1.1, 1.1, 1.1, 1.1),
        fill = category_fills
      ),
      alpha = .6,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_identity() +

    ggplot2::geom_hline(yintercept = 0.75, linewidth = 0.75) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = info$weight_cutoffs[2:4],
        xend = info$weight_cutoffs[2:4],
        y = 0,
        yend = 0.75
      ),
      linetype = "dashed",
      linewidth = 0.75
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = info$weight_cutoffs[2:4],
        xend = info$weight_cutoffs[2:4],
        y = 0.75,
        yend = 1.1
      ),
      linetype = "solid",
      linewidth = 0.75
    ) +
    add_wt_line(info) +
    add_wt_text(info) +
    ggplot2::scale_x_continuous(
      breaks = round(info$weight_cutoffs[2:4], 0),
      labels = paste0(round(info$weight_cutoffs[2:4]), " lbs"),
      expand = c(0.003, 0.003)
    ) +
    ggplot2::scale_y_continuous(expand = c(0.015, 0.015)) +
    ggplot2::labs(x = "Weight (lbs)") +
    ggplot2::coord_fixed(ratio = 40) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(
        size = 12,
        color = "black",
        margin = ggplot2::margin(t = 10)
      ),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 10, color = "black"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(
        fill = "white",
        color = "black",
        linewidth = 2
      )
    )
}
