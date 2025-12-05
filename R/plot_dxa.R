#' @title generate_body_fat_plot
#' @description Generates a bar plot of the participant's body fat percent compared to norms by age and sex.
#' @param total_fat Participant's body fat percent
#' @param subject_sex Participant's sex (1 = male, 0 = female)
#' @param subject_birthdate Date of birth (YYYY-MM-DD)
#' @param fittest_date Date of DXA scan (YYYY-MM-DD)
#' @return A ggplot object
#' @export
#' @importFrom ggplot2 geom_segment aes geom_point annotate ggplot geom_rect geom_label geom_text scale_x_continuous scale_y_continuous labs coord_fixed theme element_text element_blank margin

generate_body_fat_plot <- function(
  total_fat,
  subject_sex,
  subject_birthdate,
  fittest_date
) {
  age <-
    as.numeric(as.Date(fittest_date) - as.Date(subject_birthdate)) %/% 365.25

  get_bfp_cat_norms <- function(subject_sex, age) {
    if (subject_sex == 1) {
      if (age <= 39) {
        return(c(8, 20, 24))
      } else if (age <= 59) {
        return(c(11, 22, 27))
      } else {
        return(c(13, 25, 30))
      }
    } else {
      if (age <= 39) {
        return(c(21, 33, 39))
      } else if (age <= 59) {
        return(c(23, 34, 40))
      } else {
        return(c(24, 36, 42))
      }
    }
  }

  cutoffs <- get_bfp_cat_norms(subject_sex, age)

  add_x_axis_ticks <- function(ticks = seq(5, 65, by = 5)) {
    list(
      ggplot2::geom_segment(
        ggplot2::aes(x = ticks, y = 0.65, yend = 0.70),
        color = "black"
      ),
      ggplot2::geom_segment(
        ggplot2::aes(x = 0, xend = 70, y = 0.7, yend = 0.7),
        color = "black",
        linewidth = 1
      ),
      ggplot2::geom_text(
        ggplot2::aes(x = ticks, y = 0.55, label = ticks),
        size = 4,
        color = "black",
        fontface = "bold",
      )
    )
  }

  add_theme <- function(cutoffs) {
    list(
      ggplot2::geom_segment(
        ggplot2::aes(x = cutoffs[1:2], y = 0.7, yend = 0.95),
        linewidth = 1
      ),
      ggplot2::theme_minimal(),
      ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = 14,
          color = "black",
          face = "bold",
          margin = ggplot2::margin(r = 10)
        ),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(
          color = "black",
          linewidth = 2
        ),
        plot.background = ggplot2::element_blank()
      )
    )
  }

  ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = c(0, cutoffs[1], cutoffs[2]),
        xmax = c(cutoffs[1], cutoffs[2], 70),
        ymin = c(0.7, 0.25, 0.7),
        ymax = rep(0.95, 3)
      ),
      fill = c("#44c1de", "#5cbba2", "#094c92"),
      alpha = 0.25
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = total_fat, y = 0.4),
      color = "black",
      linewidth = 1.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = total_fat, y = 0.4),
      fill = "black",
      size = 4
    ) +
    ggplot2::geom_label(
      ggplot2::aes(x = total_fat + 3, y = 0.4),
      label = paste0(total_fat, "%"),
      fill = "white",
      fontface = "bold",
      size = 4,
      hjust = 0,
      label.size = 0.25
    ) +
    ggplot2::annotate(
      geom = "text",
      x = c(cutoffs[1] / 2, mean(c(cutoffs[1], cutoffs[2])), cutoffs[2] + 5),
      y = rep(0.825, 3),
      label = c("Low", "Healthy", "High"),
      size = 4,
      fontface = "bold"
    ) +
    add_x_axis_ticks() +
    ggplot2::scale_x_continuous(limits = c(0, 70), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.25, 0.95), expand = c(0, 0)) +
    ggplot2::labs(y = "Body Fat %", x = NULL) +
    ggplot2::coord_fixed(ratio = 15) +
    add_theme(cutoffs)
}
