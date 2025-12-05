#' @title run_ggir
#' @description Processes the ActiGraph accelerometer data with the GGIR R Package
#' @param id The patient id as represented in the study database
#' @param acceldir  The path name of the directory containing the raw .gt3x accelerometer files
#' @param outputdir The path name of the directory containing the participant feedback reports
#' @return The path name to the results of the processed accelerometer data
#' @details Processes the ActiGraph accelerometer data with the GGIR R Package
#' @seealso
#'  \code{\link[GGIR]{GGIR}}
#' @rdname run_ggir
#' @export
#' @importFrom GGIR GGIR

run_ggir <- function(id, acceldir, outputdir) {
  files <- list.files(
    acceldir,
    pattern = ".gt3x$",
    recursive = TRUE,
    full.names = TRUE
  )

  accelfile <- files[grep(gsub("RED_", "", id), files)]

  if (length(accelfile) == 0) {
    return(NULL)
  }

  acceldata <- file.path(outputdir, id, "accelerometer", "data")

  accelres <- file.path(outputdir, id, "accelerometer", "results")

  if (!dirname(acceldir) == file.path(outputdir, id)) {
    if (!dir.exists(acceldata)) {
      dir.create(acceldata, recursive = TRUE)
    }
    if (!dir.exists(accelres)) {
      dir.create(accelres, recursive = TRUE)
    }
    invisible(file.copy(
      from = accelfile,
      to = file.path(acceldata, basename(accelfile))
    ))
  }

  suppressWarnings(
    GGIR::GGIR(
      datadir = acceldata,
      outputdir = accelres,
      mode = 1:5,
      visualreport = FALSE,
      windowsizes = c(1, 900, 3600),
      mvpathreshold = 100.6,
      threshold.lig = 44.8,
      threshold.mod = 100.6,
      threshold.vig = 428.8,
      desiredtz = "UTC",
      overwrite = FALSE
    )
  )

  return(accelres)
}
