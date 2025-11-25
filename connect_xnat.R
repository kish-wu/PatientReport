# This script runs the pipeline to download, convert, and create PNGs
# for MRI data from an XNAT server. It processes all subjects listed in the
# xnat_config.R file.

# Source utility functions and handle package installation
source("./xnat_utils.R")

# Source XNAT configuration (credentials and subject list)
source("./xnat_config.R")

# --- Configuration ---
XNAT_HOST <- "https://cnda.wustl.edu/"
PROJECT_NAME <- "Alzheimers Biomarkers Consortium Down Syndrome ABC-DS"
SCAN_DESCRIPTION <- "Accelerated Sagittal IR-FSPGR"
OUTPUT_DIR <- file.path(getwd(), "output_images")

# --- Main Processing Loop ---
cat("Starting XNAT processing pipeline...\n")

# Get the project ID once
project <- get_projects_httr(XNAT_HOST, xnat_user, xnat_pass, name = PROJECT_NAME)
if (is.null(project) || nrow(project) == 0) {
  stop("Could not find the project: ", PROJECT_NAME)
}
project_id <- project$ID
cat("Found project '", PROJECT_NAME, "' with ID: ", project_id, "\n", sep = "")

# Loop through each subject defined in the config file
for (subject_label in xnat_subjects) {
  cat("\n--- Processing Subject: ", subject_label, " ---\n", sep = "")
  
  # 1. Get Subject
  subject <- get_subjects_httr(XNAT_HOST, xnat_user, xnat_pass, project_id, subject_label)
  if (is.null(subject) || nrow(subject) == 0) {
    cat("  - ERROR: Subject not found. Skipping.\n")
    next
  }
  subject_id <- subject$ID
  cat("  - Found Subject ID:", subject_id, "\n")
  
  # 2. Get Experiments and find the first MRI session
  experiments <- get_experiments_httr(XNAT_HOST, xnat_user, xnat_pass, project_id, subject_id)
  mri_sessions <- experiments[grep("mr", experiments$xsiType), ]
  if (is.null(mri_sessions) || nrow(mri_sessions) == 0) {
    cat("  - ERROR: No MRI sessions found. Skipping.\n")
    next
  }
  first_mri_session <- mri_sessions[1, ]
  experiment_id <- first_mri_session$"xnat:subjectassessordata/id"
  cat("  - Found MRI session:", experiment_id, "\n")
  
  # 3. Get Scans and find the target scan
  scans <- get_scans_httr(XNAT_HOST, xnat_user, xnat_pass, project_id, subject_id, experiment_id)
  scan_to_download <- scans[scans$series_description == SCAN_DESCRIPTION, ]
  if (is.null(scan_to_download) || nrow(scan_to_download) == 0) {
    cat("  - ERROR: Scan type '", SCAN_DESCRIPTION, "' not found. Skipping.\n", sep = "")
    next
  }
  scan_id <- scan_to_download$ID[1]
  cat("  - Found scan type '", SCAN_DESCRIPTION, "' with ID: ", scan_id, "\n", sep = "")
  
  # 4. Download Scan Data
  cat("  - Downloading scan data...\n")
  downloaded_zip <- download_scan_httr(XNAT_HOST, xnat_user, xnat_pass, project_id, subject_id, experiment_id, scan_id, OUTPUT_DIR)
  if (is.null(downloaded_zip)) {
    cat("  - ERROR: Failed to download scan data. Skipping.\n")
    next
  }
  cat("  - Download complete:", downloaded_zip, "\n")
  
  # 5. Convert to NIfTI
  cat("  - Converting DICOM to NIfTI...\n")
  nifti_file <- convert_dicom_zip_to_nifti(downloaded_zip, OUTPUT_DIR)
  if (is.null(nifti_file)) {
    cat("  - ERROR: Failed to convert to NIfTI. Skipping.\n")
    next
  }
  cat("  - Conversion successful:", nifti_file, "\n")
  
  # 6. Save NIfTI as PNG
  cat("  - Saving NIfTI as PNG images...\n")
  axial_png <- save_nifti_as_png(nifti_file, OUTPUT_DIR, subject_label, "axial")
  sagittal_png <- save_nifti_as_png(nifti_file, OUTPUT_DIR, subject_label, "sagittal")
  
  if (!is.null(axial_png) && !is.null(sagittal_png)) {
    cat("  - PNGs created:", axial_png, "and", sagittal_png, "\n")
  } else {
    cat("  - ERROR: Failed to create one or more PNG images.\n")
  }
  
  cat("--- Finished Subject: ", subject_label, " ---\n", sep = "")
}

cat("\nPipeline finished.\n")