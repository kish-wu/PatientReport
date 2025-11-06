# xnat_library.R

# --- Package Installation Check ---
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, repos = "http://cran.rstudio.com/")
      library(pkg, character.only = TRUE)
    }
  }
}
# Load RNifti first to avoid conflicts
install_if_missing(c("httr", "jsonlite", "RNifti", "oro.dicom", "oro.nifti"))

# --- Data Retrieval Functions ---
get_projects_httr <- function(host, user, pass, name = NULL) {
  url <- paste0(host, "data/projects")
  response <- httr::GET(url = url, httr::authenticate(user, pass), httr::accept_json())
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")
    projects_data <- jsonlite::fromJSON(content)
    projects_df <- projects_data$ResultSet$Result
    if (!is.null(name)) {
      projects_df <- projects_df[projects_df$name == name, ]
    }
    return(projects_df)
  } else { return(NULL) }
}
get_subjects_httr <- function(host, user, pass, project_id, subject_label = NULL) {
  url <- paste0(host, "data/projects/", project_id, "/subjects")
  response <- httr::GET(url = url, httr::authenticate(user, pass), httr::accept_json())
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")
    subjects_data <- jsonlite::fromJSON(content)
    subjects_df <- subjects_data$ResultSet$Result
    if (!is.null(subject_label)) {
      subjects_df <- subjects_df[subjects_df$label == subject_label, ]
    }
    return(subjects_df)
  } else { return(NULL) }
}
get_experiments_httr <- function(host, user, pass, project_id, subject_id) {
  url <- paste0(host, "data/projects/", project_id, "/subjects/", subject_id, "/experiments")
  response <- httr::GET(url = url, httr::authenticate(user, pass), httr::accept_json())
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")
    return(jsonlite::fromJSON(content)$ResultSet$Result)
  } else { return(NULL) }
}
get_scans_httr <- function(host, user, pass, project_id, subject_id, experiment_id) {
  url <- paste0(host, "data/projects/", project_id, "/subjects/", subject_id, "/experiments/", experiment_id, "/scans")
  response <- httr::GET(url = url, httr::authenticate(user, pass), httr::accept_json())
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")
    return(jsonlite::fromJSON(content)$ResultSet$Result)
  } else { return(NULL) }
}
download_scan_httr <- function(host, user, pass, project_id, subject_id, experiment_id, scan_id, out_dir) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_file <- file.path(out_dir, paste0(scan_id, ".zip"))
  url <- paste0(host, "data/projects/", project_id, "/subjects/", subject_id, "/experiments/", experiment_id, "/scans/", scan_id, "/files?format=zip")
  response <- httr::GET(url = url, httr::authenticate(user, pass), httr::write_disk(out_file, overwrite = TRUE))
  if (httr::status_code(response) == 200) {
    return(out_file)
  } else { return(NULL) }
}

# --- Data Processing Functions ---
convert_dicom_zip_to_nifti <- function(zip_file, out_dir) {
  tryCatch({
    unzip_dir <- file.path(out_dir, "dicom")
    if (!dir.exists(unzip_dir)) dir.create(unzip_dir, recursive = TRUE)
    unzip(zip_file, exdir = unzip_dir, overwrite = TRUE)
    
    all_files <- list.files(unzip_dir, recursive = TRUE, full.names = TRUE)
    dcm_files <- all_files[grepl("\\.dcm$", all_files, ignore.case = TRUE)]
    
    if (length(dcm_files) > 0) {
      dicom_files_path <- dirname(dcm_files[1])
    } else { return(NULL) }

    dicom_data <- oro.dicom::readDICOM(dicom_files_path, verbose = FALSE)
    nifti_data <- oro.dicom::dicom2nifti(dicom_data)
    
    nifti_filename <- paste0(basename(tools::file_path_sans_ext(zip_file)), ".nii.gz")
    nifti_filepath <- file.path(out_dir, nifti_filename)
    
    RNifti::writeNifti(nifti_data, file = nifti_filepath)
    
    if (file.exists(nifti_filepath)) {
      return(nifti_filepath)
    } else { return(NULL) }
  }, error = function(e) { return(NULL) })
}

#' @title save_nifti_as_png
#' @description Saves slices from a NIfTI file as PNG images.
save_nifti_as_png <- function(nifti_file, out_dir, subject_id, plane) {
  tryCatch({
    nifti_image <- RNifti::readNifti(nifti_file)
    
    png_filename <- file.path(out_dir, paste0(subject_id, "_", plane, ".png"))
    png(filename = png_filename, width = 600, height = 600, bg = "black")
    par(mar = c(0, 0, 0, 0))
    
    dims <- dim(nifti_image)
    
    if (plane == "axial") {
      slice_data <- nifti_image[ , , floor(dims[3] / 2)]
    } else if (plane == "sagittal") {
      slice_data <- nifti_image[floor(dims[1] / 2), , ]
    } else {
      stop("Invalid plane specified.")
    }
    
    # Use the base `image` function for matrices
    image(t(slice_data), col = grDevices::gray(0:255 / 255), axes = FALSE)
    
    dev.off()
    
    if (file.exists(png_filename)) {
      return(png_filename)
    } else { return(NULL) }
    
  }, error = function(e) {
    print(paste("An error occurred while creating the PNG for plane", plane, ":"))
    print(e$message)
    if (grDevices::dev.cur() != 1) dev.off()
    return(NULL)
  })
}

# --- Utility Functions ---
cleanup_files <- function(directory, extensions) {
  files_to_remove <- list.files(directory, pattern = paste0("\\.((", paste(extensions, collapse = "|"), "))$"), full.names = TRUE)
  sapply(files_to_remove, file.remove)
}

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
