# --- Master Pipeline Script ---
# This script runs the full BRIDGE21 reporting pipeline for one or more subjects
# using the example data provided in the project.

# Resolve project root regardless of where the script is invoked from
main_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(...) NULL)
project_root <- if (!is.null(main_file)) {
  normalizePath(file.path(dirname(main_file), ".."))
} else {
  getwd()
}
old_wd <- getwd()
setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

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

# Source necessary functions and configuration files.
source(find_first_existing("generate_report_custom.R"))
# Load XNAT credentials/subjects 
config_path <- find_first_existing("xnat_config.R")
source(config_path)

output_directory <- file.path(getwd(), "participant_reports")
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
  cat("Created output directory:", output_directory, "\n")
}
cat("Starting report generation pipeline...\n\n")

for (subject_id in xnat_subjects) {
  cat("--- Processing Subject:", subject_id, "---\n")
  # Run the generator in a fresh R session to avoid processx/quarto load issues.
  # This preserves the interactive Shiny steps (slice picker + orientation).
  status <- 0L
  report_path <- NULL
  tryCatch({
    report_path <- generate_report_custom(
      id = subject_id,
      datafile = file.path(project_root, "extdata", "toy_data.csv"),
      outputdir = output_directory,
      acceldir = NULL,
      reports = NULL,
      example_report = TRUE,
      adjust_images = TRUE
    )
  }, error = function(e) {
    status <<- 1L
    cat("  - ERROR: ", e$message, "\n", sep = "")
  })

  if (status == 0) {
    if (!is.null(report_path) && file.exists(report_path)) {
      cat("  - Successfully generated report:", report_path, "\n")
    } else {
      cat("  - Completed without error but report file not found; check logs above.\n")
    }
  }

  cat("--- Finished Subject:", subject_id, "---\n\n")
}

cat("Pipeline finished.\n")
