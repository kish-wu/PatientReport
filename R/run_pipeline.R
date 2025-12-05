# Lightweight runner to loop over subjects without the processx/Quarto issue seen in main.R.

# Resolve project root so paths work regardless of invocation directory
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

source(find_first_existing("generate_report_custom.R"))
source(find_first_existing("xnat_config.R"))

output_directory <- file.path(getwd(), "participant_reports")
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

# Use example data by default; override with DATAFILE/EXAMPLE_REPORT env vars if needed
example_report <- tolower(Sys.getenv("EXAMPLE_REPORT", "true")) %in% c("true", "1", "yes")
datafile <- Sys.getenv("DATAFILE", file.path(project_root, "extdata", "toy_data.csv"))
acceldir <- Sys.getenv("ACCELDIR", unset = "")
if (acceldir == "") acceldir <- NULL

cat("Starting run_pipeline.R\n")
cat("  project_root:", project_root, "\n")
cat("  output_directory:", output_directory, "\n")
cat("  datafile:", datafile, "\n")
cat("  example_report:", example_report, "\n")
cat("  acceldir:", ifelse(is.null(acceldir), "NULL", acceldir), "\n\n")

for (subject_id in xnat_subjects) {
  cat("--- Processing Subject:", subject_id, "---\n")
  status <- 0L
  report_path <- NULL
  tryCatch({
    report_path <- generate_report_custom(
      id = subject_id,
      datafile = datafile,
      outputdir = output_directory,
      acceldir = acceldir,
      reports = NULL,
      example_report = example_report,
      adjust_images = TRUE
    )
  }, error = function(e) {
    status <<- 1L
    cat("  - ERROR:", e$message, "\n")
  })

  if (identical(status, 0L)) {
    report_path <- if (is.null(report_path)) {
      file.path(
        output_directory,
        subject_id,
        sprintf("%s_Report.pdf", ifelse(example_report && subject_id == "10001", "test_jayhawk", subject_id))
      )
    } else {
      report_path
    }
    if (file.exists(report_path)) {
      cat("  - Successfully generated report:", report_path, "\n")
    } else {
      cat("  - Completed without error but report file not found; check logs above.\n")
    }
  }
  cat("--- Finished Subject:", subject_id, "---\n\n")
}

cat("Pipeline finished.\n")
