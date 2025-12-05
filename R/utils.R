retrieve_accel_summary <- function(data) {
  accel_df <- cbind(
    date = data$calendar_date,
    dur_spt_min = data$dur_spt_min,
    data[, grep("total.*min", colnames(data))]
  )

  accel_df$dur_day_total_MVPA_min <- accel_df$dur_day_total_MOD_min +
    accel_df$dur_day_total_VIG_min

  accel_df$dur_day_total_MOD_min <- accel_df$dur_day_total_VIG_min <- NULL

  accel_df_per <-
    data.frame(
      date = accel_df$date,
      apply(
        accel_df[, 2:5],
        2,
        FUN = function(x) x / rowSums(accel_df[, 2:5])
      ) *
        100
    )

  accel_df[, 2:5] <- round(accel_df[, 2:5])
  accel_df$date <- format(accel_df$date, "%m/%d/%Y")

  return(accel_df)
}

pet_amyloid_description <- function(
  centiloid,
  cognitive_decline = FALSE,
  petScanDate = NULL
) {
  if (centiloid >= 18 & cognitive_decline) {
    "Your amyloid is high and there are some changes in your memory and thinking."
  } else if (centiloid >= 18 & !cognitive_decline) {
    paste(
      "Your amyloid is high but you do not have changes in memory and thinking.",
      "You are at higher risk of developing memory changes.",
      "Regular exercise, a healthy diet, and keeping your brain busy are great ways to build your brain health."
    )
  } else if (centiloid < 18 & cognitive_decline) {
    glue::glue(
      "Your amyloid is not high but you have some changes in memory and thinking.",
      "Anytime you have changes in memory and thinking, it is important to talk to your doctor about what could be causing the changes.",
      "You can share with your doctor that you did not have high amyloid as of {petScanDate}.",
      "It is likely the cause is something other than Alzheimer\u0027s disease.",
      "Talk to your doctor about the things that can cause memory and thinking problems, including sleep apnea, depression, low levels of B12, or low levels of thyroid.",
      petScanDate = petScanDate,
      .sep = " "
    )
  } else {
    paste(
      "Your amyloid is not high and you do not have changes in memory thinking.",
      "This means brain changes common in Alzheimer\u0027s were not found at this time.",
      "It is possible amyloid is present in the brain but not at a high level.",
      "It is still possible to develop high amyloid or dementia in the future."
    )
  }
}


#' Retrieve Non-Missing Values from a Dataset
#'
#' @description
#' Extracts specified columns (and optionally an event column) from a dataset and drops any rows containing missing values.
#'
#' @param ... One or more unquoted column names to extract.
#' @param data A data frame or tibble containing the variables of interest.
#' @param event_name (Optional) A variable name representing an event column, used to subset data along with the specified variables.
#'
#' @return
#' A data frame containing the specified columns with all rows containing `NA` values removed.
#'
#' @details
#' This function is designed to simplify the process of subsetting and cleaning datasets, particularly for event-based data
#' (e.g., longitudinal REDCap exports). If `event_name` is supplied, the function ensures the event column is included and
#' removes rows with missing data across all selected variables. If not, only the specified variables are included.
#'
#' @examples
#' \dontrun{
#' get_database_values(age, sex, data = mydata)
#' get_database_values(age, bmi, data = mydata, event_name = redcap_event_name)
#' }
#'
#' @export
#' @importFrom tidyr drop_na
#' @importFrom rlang ensym ensyms

get_database_values <- function(..., data, event_name = NULL) {
  event_name <- try(as.character(rlang::ensym(event_name)), silent = TRUE)
  variable <- as.character(rlang::ensyms(...))
  if (!inherits(event_name, "try-error")) {
    tidyr::drop_na(data[, c(event_name, variable)])
  } else {
    tidyr::drop_na(data[, variable])
  }
}

#' Concatenate Strings Using the `+` Operator
#'
#' @description
#' Provides syntactic sugar for string concatenation, allowing the use of `+` as a shorthand for `paste0()`.
#'
#' @param x An expression using the `+` operator for string concatenation.
#'
#' @return
#' A character string resulting from concatenating the provided components.
#'
#' @details
#' This function evaluates an expression in a temporary environment where `+` is redefined
#' as `paste0()`. It enables writing cleaner, more readable string concatenations.
#'
#' @examples
#' add_string("Hello, " + "world" + "!")
#' # Returns "Hello, world!"
#'
#' @export
#' @importFrom rlang env caller_env enexpr

add_string <- function(x) {
  e <- rlang::env(
    rlang::caller_env(),
    `+` = function(x, y) paste0(x, y)
  )
  eval(rlang::enexpr(x), e)
}

#' Format Output for Quarto Documents
#'
#' @description
#' Outputs formatted LaTeX-compatible elements or wrapped text for use in Quarto documents.
#'
#' @param x A character string to format or print.
#' @param type A character string specifying the output type. Options are:
#'   \itemize{
#'     \item `"section"` — Print as a LaTeX `\section{}`.
#'     \item `"subsection"` — Print as a LaTeX `\subsection{}`.
#'     \item `"newpage"` — Insert a LaTeX `\newpage` command.
#'     \item `"strwrap"` — Print wrapped text with a specified width.
#'   }
#' @param width The maximum line width for text wrapping (default = 100).
#'
#' @return
#' No return value. Called for its side effects of printing to the console or Quarto document.
#'
#' @examples
#' format_quarto("Participant Characteristics", type = "section")
#' format_quarto("Next Section", type = "newpage")
#' format_quarto("This is some long text that should be wrapped.", type = "strwrap", width = 60)
#'
#' @export
#' @importFrom glue glue

format_quarto <- function(x = NULL, type, width = 100) {
  if (type == "section") {
    cat(glue::glue("\\section{{{x}}}"), sep = "\n")
  } else if (type == "subsection") {
    cat(glue::glue("\\subsection{{{x}}}"), sep = "\n")
  } else if (type == "newpage") {
    cat("\\newpage")
  } else if (type == "input") {
    cat(glue::glue("\\input{{{x}}}"), sep = "\n")
  } else if (type == "strwrap") {
    cat(strwrap(x, width = width), fill = TRUE)
  }
}

#' Expand Reports with Sub-Reports
#'
#' Expands a named list of reports by adding their designated sub-reports.
#' Returns a named list containing all original reports (both TRUE and FALSE)
#' plus their expanded sub-reports set to TRUE for activated parent reports.
#'
#' @param reports Named list where names are report identifiers and values are
#'   logical (TRUE/FALSE) indicating whether the report is activated.
#' @param sub_reports Named list where each name is a parent report and each value
#'   is a list/vector of sub-report suffixes. Default is `.subReports` if available
#'   in the parent environment.
#'
#' @return Named list where names are report identifiers and values are logical.
#'   Includes all original reports with their original TRUE/FALSE values, plus
#'   expanded sub-reports set to TRUE if their parent report is TRUE, or FALSE
#'   if their parent report is FALSE. Reports without sub-reports are returned
#'   unchanged.
#'
#' @examples
#' sub_reports <- list(
#'   kuadrc_blood = c("karyotype", "ptau217"),
#'   kuadrc_lifestyle = c("blood_pressure", "bmi", "dxa")
#' )
#'
#' reports <- list(
#'   kuadrc_blood = TRUE,
#'   other_report = FALSE,
#'   kuadrc_lifestyle = FALSE
#' )
#' expand_reports(reports, sub_reports)
#' # Returns: list(
#' #   kuadrc_blood = TRUE,
#' #   kuadrc_karyotype = TRUE,
#' #   kuadrc_ptau217 = TRUE,
#' #   other_report = FALSE,
#' #   kuadrc_lifestyle = FALSE,
#' #   kuadrc_blood_pressure = FALSE,
#' #   kuadrc_bmi = FALSE,
#' #   kuadrc_dxa = FALSE
#' # )
#'
#' @export

expand_reports <- function(reports, sub_reports = .subReports) {
  # Start with original reports
  result <- reports

  # Expand reports with sub-reports
  for (r in names(reports)) {
    if (r %in% names(sub_reports)) {
      prefix <- sub("_.*", "", r)
      sub_report_names <- paste0(prefix, "_", unlist(sub_reports[[r]]))

      # Add sub-reports with same TRUE/FALSE value as parent
      for (sub_name in sub_report_names) {
        result[[sub_name]] <- reports[[r]]
      }
    }
  }

  result
}
