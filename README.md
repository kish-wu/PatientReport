# BRIDGE21 Report Pipeline (Custom Wrapper)

## What this repo is for
Generate participant-facing PDF reports. It can run on real study data (pulling MRI from XNAT and processing accelerometer files) or on the bundled toy example.

## Key scripts (what/why)
- `R/generate_report_custom.R`: Orchestrator. Sources helpers, chooses sections, copies QMD project, writes `_variables.yaml`, triggers MRI Shiny apps (slice picker + orientation), then renders the PDF.
- `R/global_vars.R`: Report section definitions (`.allReports`, `.subReports`).
- `R/utils.R`: Helper functions used by QMDs (formatting, data extraction, etc.).
- `R/run_ggir.R`: Runs GGIR on `.gt3x` accelerometer files.
- `R/MRI_imgs/process_xnat_scans.R`: Pulls MRI from XNAT, converts DICOM → NIfTI → PNG.
- `R/MRI_imgs/xnat_utils.R`: HTTR helpers for XNAT REST + DICOM/NIfTI I/O.
- `R/MRI_imgs/adjust_mri_images.R`: Shiny app to pick/brighten slices from a NIfTI, saves axial/sagittal PNGs.
- `R/MRI_imgs/orient_mri_app.R`: Shiny app to rotate/flip PNGs and save orientation labels.
- `R/MRI_imgs/xnat_config.R`: Your XNAT credentials and `xnat_subjects` list.
- `qmd/BRIDGE21.qmd` + `qmd/sections/*`: Quarto project templates used for rendering.
- `extdata/toy_data.csv`: Example participant data (schema reference).

## Quick start: example run with pop-ups
1) Run (from repo root):
   ```
   Rscript -e "source('R/generate_report_custom.R'); generate_report_custom(id='10001', datafile='extdata/toy_data.csv', outputdir=file.path(getwd(),'participant_reports'), example_report=TRUE, adjust_images=TRUE)"
   ```
   - You will get two browser pop-ups: slice picker (save/close) then orientation (save/close).
   - Output: `participant_reports/test_jayhawk/test_jayhawk_Report.pdf`.

## Running on real data
1) Configure XNAT: edit `R/MRI_imgs/xnat_config.R` with `xnat_user`, `xnat_pass`, and `xnat_subjects`.
2) Prepare inputs:
   - `datafile`: your study CSV matching the `extdata/toy_data.csv` schema (IDs/dates, demographics, cognition DSMSE/MCRT/NTG-EDSD fields, imaging fields, blood markers, lifestyle/anthro).
   - `acceldir` (optional): folder containing `.gt3x` files; else set `acceldir=NULL`.
3) Run per subject:
   ```
   Rscript -e "source('R/generate_report_custom.R'); generate_report_custom(id='SUBJ001', datafile='/path/to/data.csv', outputdir=file.path(getwd(),'participant_reports'), acceldir='/path/to/gt3x' , example_report=FALSE, adjust_images=TRUE)"
   ```
   - For multiple IDs, loop over them in the same style.
   - Imaging: pulls from XNAT using your credentials; if no MRI is available, the Shiny apps are skipped.
   - If not skipped, you will get two browser pop-ups: manually do slice picker (save/close) then orientation (save/close).
   - Accelerometer: processed via GGIR if `acceldir` provided.

## What happens 
- Sources helpers (`global_vars.R`, `utils.R`, `run_ggir.R`, `process_xnat_scans.R`, `xnat_config.R`).
- Resolves which report sections to render (all by default).
- Reads CSV and subsets by `id`; validates the ID exists unless `example_report=TRUE`.
- MRI:
  - Example: copies canned PNGs/NIfTI if present.
  - Real: downloads DICOM from XNAT → converts to NIfTI → PNGs → launches slice picker → launches orientation app → saves `orientation.yaml`.
- Accelerometer: copies/runs GGIR if data provided; otherwise skipped.
- Copies QMD project to a temp dir, writes `_variables.yaml`, renders PDF, and writes outputs to `participant_reports/<id>/`.

## Outputs
- `participant_reports/<id>/<id>_Report.pdf`
- `participant_reports/<id>/_variables.yaml`
- Imaging under `participant_reports/<id>/imaging/`
- Accelerometer (if processed) under `participant_reports/<id>/accelerometer/`

## Notes
- `R/main.R` and `R/run_pipeline.R` are present but may be blocked by environment restrictions here (processx/Quarto/port binding). Use the `Rscript -e "source(...); generate_report_custom(...)"` pattern above; it works end-to-end with Shiny pop-ups and rendering.
