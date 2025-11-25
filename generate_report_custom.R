#' @title Generate Participant Feedback Report (Custom)
#' @description Generates a Quarto PDF report. This version uses our custom pipeline
#' for handling XNAT image processing.
#' @param id The patient id as represented in the study database.
#' @param datafile The path name of the file containing the data as a .csv file.
#' @param outputdir The path name for the directory to store the final report.
#' @param acceldir The path name of the directory containing raw .gt3x accelerometer files, Default = NULL.
#' @param reports A vector of report sections to generate, Default = NULL (returns all).
#' @param example_report Boolean, if TRUE, generates a sample report using example data, Default = FALSE.
#' @return The file path of the generated participant feedback report.

generate_report_custom <- function(
  id,
  datafile,
  outputdir,
  acceldir = NULL,
  reports = NULL,
  example_report = FALSE
) {
  find_file <- function(...) {
    candidates <- list(...)
    for (path in candidates) {
      if (file.exists(path)) return(path)
    }
    stop("Could not find required file among: ", paste(candidates, collapse = ", "))
  }

  # --- Initial Setup ---
  if (outputdir == getwd()) {
    stop(
      "\nCurrent working directory is: ", getwd(),
      "\n'outputdir' must be different from your current working directory."
    )
  }

  # Load necessary functions and configurations
  source(find_file("R/global_vars.R", "global_vars.R"))
  source(find_file("R/utils.R", "utils.R"))
  source(find_file("R/my code/run_ggir.R", "R/run_ggir.R", "run_ggir.R"))
  source(find_file("R/my code/process_xnat_scans.R", "process_xnat_scans.R"))
  source(find_file("R/xnat_config.R", "R/my code/xnat_config.R", "xnat_config.R"))

  # --- Report Section Logic (copied from original) ---
  allReports <- unname(unlist(purrr::imap(.allReports, ~ paste0(.y, "_", .x))))
  if (is.null(reports)) {
    reports <- allReports
  } else if (length(reports) == 1 & any(reports %in% names(.allReports))) {
    reports <- allReports[grepl(reports, allReports)]
  } else if (any(reports %in% allReports)) {
    reports <- allReports[allReports %in% reports]
  } else {
    stop("Could not match the reports.")
  }
  reports <- lapply(allReports, FUN = function(x) ifelse(x %in% reports, TRUE, FALSE))
  names(reports) <- allReports
  reports <- expand_reports(reports)

  # --- Handle Example Report Case ---
  if (example_report) {
    id <- "test_jayhawk"

    # Prefer installed package example data but fall back to local files so we
    # can run in a development checkout.
    example_data_candidates <- c(
      file.path(getwd(), "extdata", "toy_data.csv"),
      system.file("extdata/example.csv", package = "BRIDGE21"),
      file.path(getwd(), "extdata", "example.csv")
    )
    datafile <- example_data_candidates[example_data_candidates != ""][1]

    if (is.na(datafile) || datafile == "") {
      stop("Example data not found. Install BRIDGE21 or add extdata/example.csv.")
    }
  }

  # --- Data Validation ---
  data <- readr::read_csv(datafile, show_col_types = FALSE, col_select = c(ptid, coenrol_studyid))
  if (!id %in% unique(data$ptid) && !example_report) {
    stop(sprintf("Patient ID %s not found in the dataset.", id))
  }

  # --- Directory Setup ---
  persondir <- file.path(outputdir, id)
  if (!dir.exists(persondir)) dir.create(persondir, recursive = TRUE)
  imagedir <- file.path(persondir, "imaging")
  if (!dir.exists(imagedir)) dir.create(imagedir, recursive = TRUE)

  # --- Image Processing ---
  if (example_report) {
    # For the example, copy placeholder images
    # Prefer package images, otherwise fall back to local placeholders.
    mriFiles <- system.file(
      c("images/axialImage.png", "images/sagittalImage.png"),
      package = "BRIDGE21"
    )
    if (any(mriFiles == "")) {
      mriFiles <- file.path(
        getwd(),
        c("output_images/10001_axial.png", "output_images/10001_sagittal.png")
      )
    }
    if (all(file.exists(mriFiles))) {
      invisible(file.copy(from = mriFiles, to = imagedir, overwrite = TRUE))
    } else {
      cat("Could not find example MRI images. Skipping.\n")
    }
  } else {
    # Use our custom function for real data processing
    cat("--- Starting XNAT Image Processing ---\n")
    xnat_connection_config <- list(
      host = "https://cnda.wustl.edu/",
      user = xnat_user,
      pass = xnat_pass,
      project_name = "Alzheimers Biomarkers Consortium Down Syndrome ABC-DS",
      scan_description = "Accelerated Sagittal IR-FSPGR"
    )
    process_xnat_scans(
      subjects = id,
      config = xnat_connection_config,
      output_dir = imagedir
    )
    cat("--- Finished XNAT Image Processing ---\n")
  }

  # --- Accelerometer Processing ---
  if (example_report) {
    accelFolder <- system.file("extdata/accelerometer", package = "BRIDGE21")
    if (accelFolder == "") {
      accelFolder <- file.path(getwd(), "extdata", "accelerometer")
    }
    if (accelFolder != "" && dir.exists(accelFolder)) {
      file.copy(from = accelFolder, to = persondir, recursive = TRUE, overwrite = TRUE)
    } else {
      cat("Could not find example accelerometer data. Skipping activity section.\n")
    }
    accelres <- file.path(persondir, "accelerometer", "results")
  } else if (!is.null(acceldir)) {
    accelres <- run_ggir(id = id, acceldir = acceldir, outputdir = outputdir)
  } else {
    accelres <- NULL
  }

  # --- Quarto Rendering Setup ---
  # Copy the qmd project into a temporary directory for rendering
  temp_render_dir <- file.path(getwd(), paste0("temp_", id))
  if (dir.exists(temp_render_dir)) unlink(temp_render_dir, recursive = TRUE)
  dir.create(temp_render_dir)
  
  # This assumes the qmd files are in the project's 'qmd' directory
  qmd_source_dir <- file.path(getwd(), "qmd")
  invisible(file.copy(from = list.files(qmd_source_dir, full.names = TRUE), to = temp_render_dir, recursive = TRUE))

  # Write the variables YAML file
  yaml::write_yaml(
    c(
      list(
        id = id,
        datafile = datafile,
        outputdir = outputdir,
        imagedir = imagedir,
        accelres = accelres
      ),
      reports
    ),
    file = file.path(temp_render_dir, "_variables.yaml")
  )

  # --- Render the Report ---
  pdffile <- sprintf("%s_Report.pdf", id)
  old_wd <- getwd()
  setwd(temp_render_dir)
  render_output <- tryCatch(
    system2(
      command = "quarto",
      args = c("render", "BRIDGE21.qmd", "--output", pdffile),
      stdout = TRUE,
      stderr = TRUE
    ),
    finally = setwd(old_wd)
  )
  render_status <- attr(render_output, "status")
  if (!is.null(render_status) && render_status != 0) {
    stop(
      "Quarto render failed with status ", render_status, ":\n",
      paste(render_output, collapse = "\n")
    )
  }

  # --- Final File Management ---
  final_report_path <- file.path(persondir, pdffile)
  # Move the rendered PDF to the final output directory
  invisible(file.copy(from = file.path(temp_render_dir, pdffile), to = final_report_path, overwrite = TRUE))
  # Also copy the variables file for reference
  invisible(file.copy(from = file.path(temp_render_dir, "_variables.yaml"), to = file.path(persondir, "_variables.yaml"), overwrite = TRUE))

  # Clean up the temporary rendering directory
  unlink(temp_render_dir, recursive = TRUE)

  cat("Report generated successfully at:", final_report_path, "\n")
  return(final_report_path)
}
