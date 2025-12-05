#' @title generate_report_custom
#' @description Unified report generator that supports both full data processing
#' (XNAT images, GGIR accelerometer processing) and the packaged Jayhawk example.
#' @param id Participant identifier found in the study CSV.
#' @param datafile Path to the study CSV file.
#' @param outputdir Directory where participant reports should be written.
#' @param acceldir Optional directory containing raw .gt3x accelerometer files.
#' @param reports Optional vector of report sections to render; defaults to all.
#' @param example_report Logical; when TRUE uses the packaged Jayhawk example data
#'   and canned images/plots.
#' @param adjust_images Boolean, if TRUE, launch the MRI adjustment Shiny app
#'   after image creation and before rendering the PDF. Default = TRUE.
#' @return Full path to the generated PDF report.
#' @export
#' @importFrom readr read_csv
#' @importFrom yaml write_yaml
#' @importFrom quarto quarto_render
generate_report_custom <- function(
  id,
  datafile,
  outputdir,
  acceldir = NULL,
  reports = NULL,
  example_report = FALSE,
  adjust_images = TRUE
) {
  find_first_existing <- function(paths) {
    paths <- unique(c(
      paths,
      file.path("R", paths),
      file.path("R", "MRI_imgs", basename(paths))
    ))
    paths <- paths[paths != ""]
    existing <- paths[file.exists(paths)]
    if (length(existing) == 0) {
      stop("Could not find any of: ", paste(paths, collapse = ", "))
    }
    existing[[1]]
  }

  if (outputdir == getwd()) {
    stop(
      "\nCurrent working directory is: ",
      getwd(),
      "\n'outputdir' must be different from your current working directory."
    )
  }
  source(find_first_existing(c("xnat_utils.R")))
  source(find_first_existing(c("global_vars.R")))
  source(find_first_existing(c("utils.R")))
  source(find_first_existing(c("run_ggir.R")))
  source(find_first_existing(c("process_xnat_scans.R")))
  source(find_first_existing(c("xnat_config.R")))
  source(find_first_existing(c("adjust_mri_images.R")))
  source(find_first_existing(c("orient_mri_app.R")))

  allReports <- unname(unlist(purrr::imap(.allReports, ~ paste0(.y, "_", .x))))
  if (is.null(reports)) {
    reports <- allReports
  } else if (length(reports) == 1 && any(reports %in% names(.allReports))) {
    reports <- allReports[grepl(reports, allReports)]
  } else if (any(reports %in% allReports)) {
    reports <- allReports[allReports %in% reports]
  } else {
    stop("Could not match the reports.")
  }
  reports <- lapply(allReports, function(x) x %in% reports)
  names(reports) <- allReports
  reports <- expand_reports(reports)

  # Example overrides
  if (example_report) {
    id <- "test_jayhawk"
    example_data_candidates <- c(
      file.path(getwd(), "extdata", "toy_data.csv")
    )
    datafile <- find_first_existing(example_data_candidates)
  }

  id_check <- readr::read_csv(
    datafile,
    show_col_types = FALSE,
    col_select = c(ptid, coenrol_studyid)
  )
  if (!example_report && !id %in% unique(id_check$ptid)) {
    stop(sprintf("We did not find patient ID %s in the dataset", id))
  }

  # Directory setup
  persondir <- file.path(outputdir, id)
  if (!dir.exists(persondir)) dir.create(persondir, recursive = TRUE)
  imagedir <- file.path(persondir, "imaging")
  if (!dir.exists(imagedir)) dir.create(imagedir, recursive = TRUE)

  # Imaging workflow
  if (example_report) {
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
      message("Could not find example MRI images; skipping imaging copy.")
    }
  } else {
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
  }
  if (adjust_images) {
    nifti_files <- list.files(
      imagedir,
      pattern = "\\.nii(\\.gz)?$",
      full.names = TRUE,
      recursive = TRUE
    )
    if (length(nifti_files) > 0) {
      cat("--- Launching MRI Adjustment App (save the desired slices, then close the window) ---\n")
      adjust_mri_images(id = id, imagedir = imagedir)
      cat("--- MRI Adjustment complete; proceeding with report generation ---\n")
    } else {
      cat("No NIfTI files found; skipping interactive MRI adjustment.\n")
    }
  }
  if (adjust_images) {
    png_files <- list.files(
      imagedir,
      pattern = "\\.png$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(png_files) > 0) {
      cat("--- Launching MRI Orientation App (rotate/flip, then save and close) ---\n")
      orient_mri_app(imagedir = imagedir)
      cat("--- MRI Orientation complete; proceeding with report generation ---\n")
    } else {
      cat("No PNG images found; skipping orientation helper.\n")
    }
  }
  # Accelerometer workflow
  if (example_report) {
    accelFolder <- system.file("extdata/accelerometer", package = "BRIDGE21")
    if (accelFolder == "") {
      accelFolder <- file.path(getwd(), "extdata", "accelerometer")
    }
    if (accelFolder != "" && dir.exists(accelFolder)) {
      file.copy(from = accelFolder, to = persondir, recursive = TRUE, overwrite = TRUE)
    } else {
      message("Could not find example accelerometer data; skipping activity section.")
    }
    accelres <- file.path(persondir, "accelerometer", "results")
  } else if (!is.null(acceldir)) {
    accelres <- run_ggir(id = id, acceldir = acceldir, outputdir = outputdir)
  } else {
    accelres <- NULL
  }

  # QMD project copy
  qmd_candidates <- c(
    system.file("qmd", package = "BRIDGE21"),
    file.path(getwd(), "qmd")
  )
  qmd_source_dir <- find_first_existing(qmd_candidates[dir.exists(qmd_candidates)])

  temp_render_dir <- file.path(getwd(), paste0("temp_", id))
  if (dir.exists(temp_render_dir)) unlink(temp_render_dir, recursive = TRUE)
  dir.create(temp_render_dir, recursive = TRUE)
  invisible(file.copy(
    from = list.files(qmd_source_dir, full.names = TRUE),
    to = temp_render_dir,
    recursive = TRUE
  ))

  # Variables YAML for Quarto
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

  # Render
  pdffile <- sprintf("%s_Report.pdf", id)
  old_wd <- getwd()
  setwd(temp_render_dir)
  on.exit(setwd(old_wd), add = TRUE)
  render_ok <- FALSE
  render_err <- NULL
  tryCatch({
    quarto::quarto_render(
      input = "BRIDGE21.qmd",
      output_file = pdffile
    )
    render_ok <- TRUE
  }, error = function(e) {
    render_err <<- e
  })

  if (!render_ok) {
    message("quarto::quarto_render() failed (", render_err$message, "); retrying via quarto CLI...")
    qbin <- Sys.which("quarto")
    if (qbin == "") {
      stop("Quarto CLI not found on PATH and quarto::quarto_render() failed.")
    }
    status <- system2(
      qbin,
      c("render", "BRIDGE21.qmd", "--output", pdffile),
      stdout = "",
      stderr = ""
    )
    if (!identical(status, 0L)) {
      stop("Quarto CLI render failed with status ", status)
    }
  }

  # Final file moves
  final_report_path <- file.path(persondir, pdffile)
  invisible(file.copy(
    from = file.path(temp_render_dir, pdffile),
    to = final_report_path,
    overwrite = TRUE
  ))
  invisible(file.copy(
    from = file.path(temp_render_dir, "_variables.yaml"),
    to = file.path(persondir, "_variables.yaml"),
    overwrite = TRUE
  ))

  unlink(temp_render_dir, recursive = TRUE)
  return(final_report_path)
}
