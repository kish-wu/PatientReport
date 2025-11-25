# --- Master Pipeline Script ---
# This script runs the full BRIDGE21 reporting pipeline for one or more subjects
# using the example data provided in the project.


# Source all necessary functions and configuration files.
source("R/xnat_utils.R")
source("R/my code/generate_report_custom.R") 
# Load XNAT credentials/subjects 
config_path <- if (file.exists("R/xnat_config.R")) {
  "R/xnat_config.R"
} else {
  "R/my code/xnat_config.R"
}
source(config_path) 

# Define the main output directory for all reports.
output_directory <- file.path(getwd(), "participant_reports")
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
  cat("Created output directory:", output_directory, "\n")
}

# --- 3. Main Processing Loop ---
# Iterate through the subjects defined in xnat_config.R and generate a report for each.
cat("Starting report generation pipeline...\n\n")

for (subject_id in xnat_subjects) {
  cat("--- Processing Subject:", subject_id, "---\n")
  
  # For the purpose of this example, we use the built-in example data.
  # The `datafile` argument is handled internally by `generate_report`
  # when `example_report = TRUE`.
  
  tryCatch({
    report_path <- generate_report_custom(
      id = subject_id,
      datafile = NULL, # Not needed for example report
      outputdir = output_directory,
      acceldir = NULL, # No example accelerometer data to process this way
      reports = NULL, # Generate all available report sections
      example_report = TRUE
    )
    
    if (!is.null(report_path) && file.exists(report_path)) {
      cat("  - Successfully generated report:", report_path, "\n")
    } else {
      cat("  - Report generation failed for an unknown reason.\n")
    }
    
  }, error = function(e) {
    cat("  - ERROR: An error occurred while processing subject", subject_id, ":\n")
    cat("    ", e$message, "\n")
  })
  
  cat("--- Finished Subject:", subject_id, "---\n\n")
}

cat("Pipeline finished.\n")

# --- 4. Manual Image Adjustment (Optional) ---
# The pipeline can be paused here to allow for manual adjustment of the MRI images
# using the Shiny app.

# To adjust images for a specific subject:
# 1. Uncomment the lines below.
# 2. Replace '10001' with the ID of the subject you want to adjust.
# 3. Run the script. The Shiny app will launch.

# cat("To manually adjust MRI images, uncomment the following lines in this script.\n")
# source("R/my code/adjust_mri_images.R")
# subject_to_adjust <- "10001"
# image_directory <- file.path(output_directory, subject_to_adjust, "imaging")
# if (dir.exists(image_directory)) {
#   adjust_mri_images(id = subject_to_adjust, imagedir = image_directory)
# } else {
#   cat("Image directory not found for subject:", subject_to_adjust, "\n")
# }
