#' @title create_blood_pressure_table
#' @description Creates a table containing systolic and diastolic blood pressure
#'     values and their respective risk categories.
#' @return A table containing blood pressure and risk categories
#' @details Creates a table containing systolic and diastolic blood pressure
#'     values and their respective risk categories.
#' @rdname create_blood_pressure_table
#' @keywords internal

create_blood_pressure_table <- function() {
  data.frame(
    systolic = c(90, 120, 130, 180),
    diastolic = c(60, 80, 90, 180),
    categories = c("Low BP", "Normal", "Elevated", "High"),
    colors = c("#ADD8E6", "#90EE90", "#FDEE8C", "#FF7F7F")
  )
}

#' @title classify_blood_pressure
#' @description Classifies systolic or diastolic blood pressure and returns the
#'     level of associated risk.
#' @return A risk category based on the value and type of blood pressure measure.
#' @details Classifies systolic or diastolic blood pressure and returns the
#'     level of associated risk.
#' @rdname classify_blood_pressure
#' @keywords internal

classify_blood_pressure <- function(value, type = c("systolic", "diastolic")) {
  type = match.arg(type)
  bptab <- create_blood_pressure_table()
  cut(
    value,
    right = FALSE,
    breaks = c(-Inf, bptab[[type]][1:3], Inf),
    labels = bptab$categories
  )
}

#' @title round_rectangle
#' @description A convenience function to plot the rounded rectangles in generate_bp_plot
#' @inheritParams grid::roundrectGrob
#' @param fill Fill color of the rectangle
#' @return An annotation_custom ggplot2 layer
#' @details A convenience function to plot the rounded rectangles in generate_bp_plot
#' @seealso
#'  \code{\link[ggplot2]{annotation_custom}}
#'  \code{\link[grid]{roundrect}}, \code{\link[grid]{gpar}}
#' @rdname round_rectangle
#' @keywords internal
#' @importFrom ggplot2 annotation_custom
#' @importFrom grid roundrectGrob gpar unit

round_rectangle <- function(x, y, width, height, radius, fill) {
  ggplot2::annotation_custom(
    grid::roundrectGrob(
      x = x,
      y = y,
      width = width,
      height = height,
      r = grid::unit(radius, "npc"), # Corner rounding
      gp = grid::gpar(fill = fill, col = NA) # Fill color with no border
    )
  )
}


#' @title generate_bp_plot
#' @description Generate a plot containing the systolic and diastolic blood pressure values
#' @param bpsys Systolic Blood Pressure
#' @param bpdia Diastolic Blood Pressure
#' @return A ggplot2 object containing the plotted blood pressure values
#' @details Generate a plot containing the systolic and diastolic blood pressure values
#' @seealso
#'  \code{\link[magick]{editing}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_raster}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{annotation_custom}}, \code{\link[ggplot2]{annotate}}, \code{\link[ggplot2]{geom_label}}, \code{\link[ggplot2]{ggtheme}}
#'  \code{\link[grid]{grid.raster}}, \code{\link[grid]{patterns}}
#' @rdname generate_bp_plot
#' @export
#' @importFrom magick image_read
#' @importFrom ggplot2 ggplot geom_rect aes annotation_custom annotate geom_label geom_text theme_void
#' @importFrom grid rasterGrob linearGradient

generate_bp_plot <- function(bpsys, bpdia) {
  bptab <- create_blood_pressure_table()

  img <- magick::image_read(system.file(
    "images/blood_pressure.png",
    package = "BRIDGE21"
  ))

  ggplot2::ggplot() +
    # Base Plot
    ggplot2::geom_rect(
      ggplot2::aes(xmin = 0, xmax = 100, ymin = 30, ymax = 220),
      fill = "white"
    ) +
    ggplot2::annotation_custom(grid::rasterGrob(
      img,
      width = 0.5,
      height = 0.25,
      x = 0.3,
      y = 0.15
    )) +
    round_rectangle(
      x = 0.75,
      y = 0.5,
      width = 0.35,
      height = 0.97,
      radius = 0.2,
      fill = grid::linearGradient(
        c("gray90", "black"),
        x1 = 0,
        y1 = 0,
        x2 = 1,
        y2 = 1
      )
    ) +
    round_rectangle(
      x = 0.75,
      y = 0.5,
      width = 0.32,
      height = 0.93,
      radius = 0.2,
      fill = "white"
    ) +
    # Systolic
    round_rectangle(
      x = 0.67,
      y = 0.18,
      width = 0.075,
      height = 0.2,
      radius = 0.5,
      fill = bptab$colors[[1]]
    ) +
    round_rectangle(
      x = 0.67,
      y = 0.78,
      width = 0.075,
      height = 0.2,
      radius = 0.5,
      fill = bptab$colors[[4]]
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = 64.6,
        xmax = 72.8,
        ymin = 60,
        ymax = bptab$systolic[[1]]
      ),
      fill = bptab$colors[[1]]
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = 64.6,
        xmax = 72.8,
        ymin = bptab$systolic[[1]],
        ymax = bptab$systolic[[2]]
      ),
      fill = bptab$colors[[2]]
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = 64.6,
        xmax = 72.8,
        ymin = bptab$systolic[[2]],
        ymax = bptab$systolic[[3]]
      ),
      fill = bptab$colors[[3]]
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = 64.6,
        xmax = 72.8,
        ymin = bptab$systolic[[3]],
        ymax = bptab$systolic[[4]]
      ),
      fill = bptab$colors[[4]]
    ) +
    ggplot2::annotate(
      "text",
      x = 68.5,
      y = 212,
      label = "SYS\n(mmHg)",
      fontface = "bold"
    ) +
    ggplot2::annotate(
      "segment",
      x = 64.6,
      xend = 72.8,
      y = bpsys,
      yend = bpsys,
      linetype = "dashed"
    ) +
    ggplot2::geom_label(
      ggplot2::aes(x = 68.5, y = bpsys + 7),
      label = bpsys,
      fontface = "bold",
      fill = "white"
    ) +
    # Diastolic
    round_rectangle(
      x = 0.84,
      y = 0.78,
      width = 0.075,
      height = 0.2,
      radius = 0.5,
      fill = bptab$colors[[4]]
    ) +
    round_rectangle(
      x = 0.84,
      y = 0.18,
      width = 0.075,
      height = 0.2,
      radius = 0.5,
      fill = bptab$colors[[1]]
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = 83.27,
        xmax = 91.5,
        ymin = bptab$diastolic[[1]],
        ymax = bptab$diastolic[[2]]
      ),
      fill = bptab$colors[[2]]
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = 83.27,
        xmax = 91.5,
        ymin = bptab$diastolic[[2]],
        ymax = bptab$diastolic[[3]]
      ),
      fill = bptab$colors[[3]]
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = 83.27,
        xmax = 91.5,
        ymin = bptab$diastolic[[3]],
        ymax = bptab$diastolic[[4]]
      ),
      fill = bptab$colors[[4]]
    ) +
    ggplot2::annotate(
      "text",
      x = 87.5,
      y = 212,
      label = "DIA\n(mmHg)",
      fontface = "bold"
    ) +
    ggplot2::annotate(
      "segment",
      x = 83.27,
      xend = 91.5,
      y = bpdia,
      yend = bpdia,
      linetype = "dashed"
    ) +
    ggplot2::geom_label(
      ggplot2::aes(x = 87.5, y = bpdia + 7),
      label = bpdia,
      fontface = "bold",
      fill = "white"
    ) +
    # Numeric Axis Labels
    ggplot2::annotate(
      "segment",
      x = 74,
      xend = 76,
      y = seq(50, 190, 5),
      yend = seq(50, 190, 5)
    ) +
    ggplot2::annotate(
      "segment",
      x = 80,
      xend = 82,
      y = seq(50, 190, 5),
      yend = seq(50, 190, 5)
    ) +
    ggplot2::annotate(
      "text",
      x = 78,
      y = seq(50, 190, 10),
      label = seq(50, 190, 10)
    ) +
    # Add legend
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = rep(3, 4),
        xmax = rep(5, 4),
        ymin = seq(90, 120, 10),
        ymax = seq(94, 129, 10)
      ),
      fill = bptab$colors
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = rep(7, 4), y = seq(90, 120, 10)),
      label = bptab$categories,
      hjust = 0,
      vjust = 0,
      size = 4
    ) +
    ggplot2::theme_void()
}