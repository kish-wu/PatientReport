 # Report Generation Custom Pipeline 

  ## Overview
  This folder holds our custom BRIDGE21 reporting pipeline. Adapted from https://github.com/bhelsel/BRIDGE21.git

  ## Key Scripts
  - main.R — Entry point. Loops over `xnat_subjects` (from xnat_config.R), calls `generate_report_custom()`,
  and writes to `participant_reports/<id>/`.
  - connet.xnat.R - Provides helpers to establish a connection/session to the XNAT host using stored credentials.
  - generate_report_custom.R — Core orchestrator. Sources helpers, selects report sections, handles images
  (example placeholders or XNAT), accelerometer paths, writes `_variables.yaml`, and invokes Quarto CLI to
  produce the PDF.
  - xnat_config.R — User/Pass and `xnat_subjects` list.
  - process_xnat_scans.R — XNAT pull: project/subject/experiment/scan lookup → download zip → unzip DICOM →
  convert to NIfTI → axial/sagittal PNGs.
  - xnat_utils.R — HTTR helpers for XNAT REST and DICOM/NIfTI I/O.
  - run_ggir.R — Runs GGIR on `.gt3x` accelerometer data and writes `accelerometer/data` and `accelerometer/
  results`.
  - utils.R — Helpers (`get_database_values`, `add_string`, `format_quarto`, `expand_reports`, accelerometer
  summary).
  - global_vars.R — Report section definitions (`.allReports`, `.subReports`).
  - Plotting helpers (used by QMDs): `plot_blood_pressure.R`, `plot_time_series_by_date.R`,
  `plot_bmi_weight.R`, `plot_dxa.R`, `save_xnat_images.R`, etc.

  ## Data Inputs
  - Participant CSV: see `extdata/toy_data.csv` for schema (~102 cols). Required fields include:
    - IDs/dates: `ptid`, `coenrol_studyid`, `redcap_event_name`, `redcap_repeat_instrument`, `visitmo/
  visitday/visityr`, `subject_birthdate`, `final_impression`.
    - Demographics: `subject_fname`, `subject_lname`, `sex`.
    - Cognition: DSMSE (`dsmsett2`, `dsvistt2`, `dssmtot2`, `dsmmtot2`, `dslgtot2`), MCRT (`dscrttfr`,
  `dsccttcr`), NTG-EDSD items `dsadl1`–`dsother6`.
    - Imaging: `scan_date`, `radiology_read`, `centiloid_apet`, `img_scan_date_apet`.
    - Blood: `ptau217_date`, `ptau217_pgml`, `dschrom`, `chromsm_source`, `ku_apoe1`, `ku_apoe2`.
    - Lifestyle/anthro: `bpsys`, `bpdias`, `height`, `weight`, `fittest_date`, `mass`, `lean`, `fat`,
  `total_fat`, `dxa_bmd_z`.
  - Accelerometer (optional): folder with `.gt3x` files or the bundled example at `extdata/accelerometer`.
  - MRI images:
    - images pulled from XNAT 


  ## Quarto Project
  - Source: `qmd/BRIDGE21.qmd` plus `qmd/sections/...`.
  - Template/theme: `_extensions/BRIDGE21/mytemplate.tex`, logos in `_extensions/BRIDGE21/logos/`.
  - Uses installed R package `BRIDGE21` (install from repo root: `Rscript -e "install.packages('.',
  repos=NULL, type='source')"`).

  ## Workflow 
    - Install the package from the repo root (once):
    Rscript -e "install.packages('.', repos=NULL, type='source')"
    - Ensure Quarto CLI is installed and on PATH (quarto --version).

    #### Configure XNAT credentials and subjects

  - Edit R/xnat_config.R 
      - xnat_user <- "<your username>"
      - xnat_pass <- "<your password>"
      - xnat_subjects <- c("SUBJ001", "SUBJ002", ...)
  - Optional: if you want to sanity-check credentials and pre-download images, run Rscript -e "source('R/connect_xnat.R')":
      - What it does: uses xnat_utils.R HTTR helpers to log in with your user/pwd, resolve the project
        (Alzheimers Biomarkers Consortium Down Syndrome ABC-DS), pull each subject’s MRI scan (series
        “Accelerated Sagittal IR-FSPGR”), download the DICOM zip, convert to NIfTI, and save axial/sagittal
        PNGs to output_images/. This is a standalone prefetch step; the main pipeline also downloads images
        automatically if you skip this.
        #### Prepare your real CSV

  - Provide a CSV with the expected schema (same columns as extdata/example.csv): demographics (name,
    birthdate, visit date, sex, final_impression), cognition (DSMSE/MCRT fields, NTG-EDSD dsadl1–
    dsother6), imaging fields (scan_date, radiology_read, centiloid_apet, img_scan_date_apet), blood markers
    (ptau217_date, ptau217_pgml, dschrom, chromsm_source, ku_apoe1/2), lifestyle/anthro (bpsys, bpdias,
    height, weight, fittest_date, mass, lean, fat, total_fat, dxa_bmd_z), etc. 
    (e.g., /path/to/data.csv).

       Prepare accelerometer input

  - If you have .gt3x files, place them in a folder and note the path (to pass as acceldir). If none, set
    acceldir = NULL to skip activity plots.

        #### Run the pipeline

  - From the repo root, in R:

    source("R/main.R")  # or directly call generate_report_custom for a single ID:
    report_path <- generate_report_custom(
      id = "SUBJ001",
      datafile = "/path/to/data.csv",
      outputdir = file.path(getwd(), "participant_reports"),
      acceldir = "/path/to/gt3x/folder",  # or NULL
      reports = NULL,                     # all sections
      example_report = FALSE              # real data + XNAT pull
    )
  - What happens inside:
      - Sources helpers (global_vars.R, utils.R, run_ggir.R, process_xnat_scans.R, xnat_config.R).
      - Resolves reports to render (all by default).
      - Reads your CSV, subsets by id.
      - Downloads MRI via XNAT (using your user/pwd) and converts to axial/sagittal PNGs in
        participant_reports/<id>/imaging/.
      - Processes accelerometer if acceldir is provided; otherwise skips.
      - Writes _variables.yaml and calls Quarto CLI to render the PDF.

    #### Render and outputs

  - The pipeline writes the PDF to participant_reports/<id>/<id>_Report.pdf and keeps _variables.yaml
    alongside it. Imaging PNGs live under participant_reports/<id>/imaging/; accelerometer outputs (if any)
    under participant_reports/<id>/accelerometer/.
