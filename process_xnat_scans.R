#' @title Process XNAT Scans
#' @description This script provides a function to run the full pipeline to download,
#' convert, and create PNGs for MRI data from an XNAT server for a list of subjects.
#'
#' @param subjects A character vector of subject labels to process.
#' @param config A list containing XNAT connection and project details.
#'   Expected elements:
#'   - `host`: URL of the XNAT server (e.g., "https://cnda.wustl.edu/").
#'   - `user`: Your XNAT username.
#'   - `pass`: Your XNAT password.
#'   - `project_name`: The name of the XNAT project.
#'   - `scan_description`: The description of the scan to download (e.g., "Accelerated Sagittal IR-FSPGR").
#' @param output_dir The directory where output images and files will be saved.
#'
#' @return Invisibly returns NULL. The function's primary purpose is to download
#'   and process files, saving them to the specified output directory.
#'
process_xnat_scans <- function(subjects, config, output_dir) {
  # --- Main Processing Loop ---
  cat("Starting XNAT processing pipeline...\n")

  # Get the project ID once
  project <- get_projects_httr(config$host, config$user, config$pass, name = config$project_name)
  if (is.null(project) || nrow(project) == 0) {
    stop("Could not find the project: ", config$project_name)
  }
  project_id <- project$ID
  cat("Found project '", config$project_name, "' with ID: ", project_id, "\n", sep = "")

  # Loop through each subject
  for (subject_label in subjects) {
    cat("\n--- Processing Subject: ", subject_label, " ---\n", sep = "")

    # 1. Get Subject
    subject <- get_subjects_httr(config$host, config$user, config$pass, project_id, subject_label)
    if (is.null(subject) || nrow(subject) == 0) {
      cat("  - ERROR: Subject not found. Skipping.\n")
      next
    }
    subject_id <- subject$ID
    cat("  - Found Subject ID:", subject_id, "\n")

    # 2. Get Experiments and find the first MRI session
    experiments <- get_experiments_httr(config$host, config$user, config$pass, project_id, subject_id)
    mri_sessions <- experiments[grep("mr", experiments$xsiType), ]
    if (is.null(mri_sessions) || nrow(mri_sessions) == 0) {
      cat("  - ERROR: No MRI sessions found. Skipping.\n")
      next
    }
    first_mri_session <- mri_sessions[1, ]
    experiment_id <- first_mri_session$"xnat:subjectassessordata/id"
    cat("  - Found MRI session:", experiment_id, "\n")

    # 3. Get Scans and find the target scan
    scans <- get_scans_httr(config$host, config$user, config$pass, project_id, subject_id, experiment_id)
    scan_to_download <- scans[scans$series_description == config$scan_description, ]
    if (is.null(scan_to_download) || nrow(scan_to_download) == 0) {
      cat("  - ERROR: Scan type '", config$scan_description, "' not found. Skipping.\n", sep = "")
      next
    }
    scan_id <- scan_to_download$ID[1]
    cat("  - Found scan type '", config$scan_description, "' with ID: ", scan_id, "\n", sep = "")

    # 4. Download Scan Data
    cat("  - Downloading scan data...\n")
    downloaded_zip <- download_scan_httr(config$host, config$user, config$pass, project_id, subject_id, experiment_id, scan_id, output_dir)
    if (is.null(downloaded_zip)) {
      cat("  - ERROR: Failed to download scan data. Skipping.\n")
      next
    }
    cat("  - Download complete:", downloaded_zip, "\n")

    # 5. Convert to NIfTI
    cat("  - Converting DICOM to NIfTI...\n")
    nifti_file <- convert_dicom_zip_to_nifti(downloaded_zip, output_dir)
    if (is.null(nifti_file)) {
      cat("  - ERROR: Failed to convert to NIfTI. Skipping.\n")
      next
    }
    cat("  - Conversion successful:", nifti_file, "\n")

    # 6. Save NIfTI as PNG
    cat("  - Saving NIfTI as PNG images...\n")
    axial_png <- save_nifti_as_png(nifti_file, output_dir, subject_label, "axial")
    sagittal_png <- save_nifti_as_png(nifti_file, output_dir, subject_label, "sagittal")

    if (!is.null(axial_png) && !is.null(sagittal_png)) {
      cat("  - PNGs created:", axial_png, "and", sagittal_png, "\n")
    } else {
      cat("  - ERROR: Failed to create one or more PNG images.\n")
    }

    cat("--- Finished Subject: ", subject_label, " ---\n", sep = "")
  }

  cat("\nPipeline finished.\n")
  invisible(NULL)
}