#' Calculate NTG-EDSD Domain Scores
#'
#' @description
#' Computes domain-level or total NTG-EDSD scores from a dataset by summing items
#' coded as 1 or 2, indicating the presence of a change. The function returns a
#' tibble containing a single column named after the provided domain label.
#'
#' @param data A data frame containing NTG-EDSD item responses. Column names
#'   should include substrings corresponding to each NTG-EDSD domain (e.g.,
#'   `"adl"`, `"lang"`, `"sleep"`, `"mem"`).
#' @param domain A short domain identifier (e.g., `"adl"`, `"lang"`, `"mem"`).
#'   When `"all"`, the function uses all columns to calculate a total score.
#' @param label A descriptive name for the output column (e.g.,
#'   `"Activities of Daily Living"`). Typically supplied automatically when used
#'   within a mapping call such as `purrr::imap_dfc()`.
#'
#' @details
#' Each domain score is calculated as the number of items coded as `1` or `2`
#' within the corresponding set of columns. Missing values are ignored when
#' summing.
#'
#' This function is typically used inside an iterative call such as
#' `purrr::imap_dfc()` to compute all NTG-EDSD domain scores at once.
#'
#' @return
#' A one-column tibble containing the summed score for the specified domain, with
#' the column name set to the provided `label`.
#'
#' @examples
#' \dontrun{
#' ntgedsd_domains <- c(
#'   all = "Total Score",
#'   adl = "Activities of Daily Living",
#'   mem = "Memory"
#' )
#'
#' purrr::imap_dfc(ntgedsd_domains, function(label, domain) {
#'   calculate_ntgedsd(ntgedsd, domain, label)
#' })
#' }
#'
#' @export
#' @importFrom rlang `:=`
#' @importFrom dplyr mutate across everything
#' @importFrom tibble tibble

calculate_ntgedsd <- function(data, domain, label) {
  `:=` <- rlang::`:=`
  if (domain == "all") {
    cols <- 1:ncol(data)
  } else {
    cols <- grep(domain, names(data))
  }

  score <-
    dplyr::mutate(
      data[, cols, drop = FALSE],
      dplyr::across(
        dplyr::everything(),
        ~ as.integer(.x %in% c(1, 2))
      )
    )

  score <- rowSums(score, na.rm = TRUE)

  tibble::tibble(!!label := score)
}
