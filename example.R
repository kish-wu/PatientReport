# example.R
# This script demonstrates how to use the xnat_library.R to download and process XNAT data,
# and then optionally use the adjust_mri_images.R script to interactively adjust the images.

# Source the library
source("./xnat_library.R")

# Source the image adjustment utility
source("./adjust_mri_images.R")

# Source the configuration
source("./xnat_config.R")

# --- Configuration ---
XNAT_HOST <- "https://cnda.wustl.edu/"
PROJECT_NAME <- "Alzheimers Biomarkers Consortium Down Syndrome ABC-DS"
SCAN_DESCRIPTION <- "Accelerated Sagittal IR-FSPGR"
OUTPUT_DIR <- file.path(getwd(), "output_images")

# Create a config list to pass to the processing function
config <- list(
  host = XNAT_HOST,
  user = xnat_user,
  pass = xnat_pass,
  project_name = PROJECT_NAME,
  scan_description = SCAN_DESCRIPTION
)

# Run the processing pipeline
process_xnat_scans(subjects = xnat_subjects, config = config, output_dir = OUTPUT_DIR)

# --- Interactive Image Adjustment (Optional) ---
# After running the pipeline
# adjust the generated images.
#
# To use it, uncomment the line below and run this script.
# adjust_mri_images(id = xnat_subjects[1], imagedir = OUTPUT_DIR)