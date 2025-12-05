#' Launch a small Shiny helper to rotate/flip MRI snapshots and set labels
#'
#' @param imagedir Directory containing axial/sagittal PNGs (defaults to working dir)
#' @param axial_pattern Regex used to detect axial image
#' @param sagittal_pattern Regex used to detect sagittal image
#'
#' The app previews transformations, lets the user adjust orientation, and saves
#' (1) rotated/flipped PNGs in-place and (2) an orientation.yaml used in the report.
#'
orient_mri_app <- function(
  imagedir = getwd(),
  axial_pattern = "axial.*\\.png$",
  sagittal_pattern = "sagittal.*\\.png$"
) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Please install.packages('shiny') to use the orientation helper.")
  }
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Please install.packages('magick') to use the orientation helper.")
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Please install.packages('yaml') to save orientation metadata.")
  }
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop("Please install.packages('base64enc') for image preview.")
  }

  find_image <- function(pattern) {
    files <- list.files(imagedir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
    if (length(files) < 1) stop(sprintf("Could not find an image matching '%s' in %s", pattern, imagedir))
    files[1]
  }

  axial_path <- find_image(axial_pattern)
  sagittal_path <- find_image(sagittal_pattern)

  axial_labels_base <- c(top = "Front", right = "", bottom = "Back", left = "")
  sagittal_labels_base <- c(top = "", right = "Back", bottom = "", left = "Front")

  rotate_labels <- function(labels, rotation) {
    # rotation is one of 0, 90, 180, 270 (clockwise)
    switch(
      as.character(rotation),
      `0`   = labels,
      `90`  = c(top = labels["left"], right = labels["top"], bottom = labels["right"], left = labels["bottom"]),
      `180` = c(top = labels["bottom"], right = labels["left"], bottom = labels["top"], left = labels["right"]),
      `270` = c(top = labels["right"], right = labels["bottom"], bottom = labels["left"], left = labels["top"]),
      labels
    )
  }

  flip_labels <- function(labels, flip_horizontal = FALSE, flip_vertical = FALSE) {
    out <- labels
    if (flip_horizontal) {
      out[c("left", "right")] <- out[c("right", "left")]
    }
    if (flip_vertical) {
      out[c("top", "bottom")] <- out[c("bottom", "top")]
    }
    out
  }

  apply_transform <- function(img, rotation, flip_h, flip_v) {
    img <- magick::image_rotate(img, rotation)
    if (isTRUE(flip_h)) img <- magick::image_flop(img)
    if (isTRUE(flip_v)) img <- magick::image_flip(img)
    img
  }

  annotate_and_encode <- function(img, labels) {
    # Add padding so text does not overlap image
    img2 <- magick::image_border(img, color = "white", geometry = "10x10")
    if (nzchar(labels["top"]))    img2 <- magick::image_annotate(img2, labels["top"], gravity = "north", size = 24, color = "black", location = "+0+10")
    if (nzchar(labels["bottom"])) img2 <- magick::image_annotate(img2, labels["bottom"], gravity = "south", size = 24, color = "black", location = "+0+10")
    if (nzchar(labels["left"]))   img2 <- magick::image_annotate(img2, labels["left"], gravity = "west",  size = 24, color = "black", location = "+10+0")
    if (nzchar(labels["right"]))  img2 <- magick::image_annotate(img2, labels["right"], gravity = "east",  size = 24, color = "black", location = "+10+0")
    raw_png <- magick::image_write(img2, format = "png")
    uri <- paste0("data:image/png;base64,", base64enc::base64encode(raw_png))
    uri
  }

  ui <- shiny::fluidPage(
    shiny::titlePanel("MRI Orientation Helper"),
    shiny::p(sprintf("Images loaded from: %s", imagedir)),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::h4("Axial"),
        shiny::uiOutput("axialImg"),
        shiny::checkboxInput("axial_flip_h", "Flip left/right", value = FALSE),
        shiny::checkboxInput("axial_flip_v", "Flip top/bottom", value = FALSE),
        shiny::selectInput("axial_rotation", "Rotate (clockwise)", choices = c(0, 90, 180, 270), selected = 0),
        shiny::verbatimTextOutput("axialLabels", placeholder = TRUE)
      ),
      shiny::column(
        width = 6,
        shiny::h4("Sagittal"),
        shiny::uiOutput("sagittalImg"),
        shiny::checkboxInput("sagittal_flip_h", "Flip left/right", value = FALSE),
        shiny::checkboxInput("sagittal_flip_v", "Flip top/bottom", value = FALSE),
        shiny::selectInput("sagittal_rotation", "Rotate (clockwise)", choices = c(0, 90, 180, 270), selected = 0),
        shiny::verbatimTextOutput("sagittalLabels", placeholder = TRUE)
      )
    ),
    shiny::hr(),
    shiny::actionButton("save", "Save rotated images + labels", class = "btn-primary"),
    shiny::verbatimTextOutput("save_status", placeholder = TRUE)
  )

  server <- function(input, output, session) {
    axial_orig <- magick::image_read(axial_path)
    sagittal_orig <- magick::image_read(sagittal_path)

    axial_transformed <- shiny::reactive({
      apply_transform(axial_orig, as.numeric(input$axial_rotation), input$axial_flip_h, input$axial_flip_v)
    })
    sagittal_transformed <- shiny::reactive({
      apply_transform(sagittal_orig, as.numeric(input$sagittal_rotation), input$sagittal_flip_h, input$sagittal_flip_v)
    })

    axial_labels <- shiny::reactive({
      lbls <- rotate_labels(axial_labels_base, as.numeric(input$axial_rotation))
      lbls <- flip_labels(lbls, input$axial_flip_h, input$axial_flip_v)
      lbls
    })
    sagittal_labels <- shiny::reactive({
      lbls <- rotate_labels(sagittal_labels_base, as.numeric(input$sagittal_rotation))
      lbls <- flip_labels(lbls, input$sagittal_flip_h, input$sagittal_flip_v)
      lbls
    })

    output$axialImg <- shiny::renderUI({
      uri <- annotate_and_encode(axial_transformed(), axial_labels())
      shiny::tags$img(src = uri, style = "max-width:100%; height:auto; border:1px solid #ccc;")
    })

    output$sagittalImg <- shiny::renderUI({
      uri <- annotate_and_encode(sagittal_transformed(), sagittal_labels())
      shiny::tags$img(src = uri, style = "max-width:100%; height:auto; border:1px solid #ccc;")
    })

    output$axialLabels <- shiny::renderText({
      lbls <- axial_labels()
      sprintf("Top: %s | Bottom: %s", lbls["top"], lbls["bottom"])
    })
    output$sagittalLabels <- shiny::renderText({
      lbls <- sagittal_labels()
      sprintf("Left: %s | Right: %s", lbls["left"], lbls["right"])
    })

    shiny::observeEvent(input$save, {
      magick::image_write(axial_transformed(), path = axial_path, format = "png")
      magick::image_write(sagittal_transformed(), path = sagittal_path, format = "png")
      yaml::write_yaml(
        list(
          axial = as.list(axial_labels()),
          sagittal = as.list(sagittal_labels())
        ),
        file.path(imagedir, "orientation.yaml")
      )
      output$save_status <- shiny::renderText({
        sprintf(
          "Saved rotated images and labels to %s (orientation.yaml updated). Re-render the report to see changes.",
          imagedir
        )
      })
      shiny::stopApp() # close the app and return to pipeline
    })
  }

  app <- shiny::shinyApp(ui, server)
  # Ensure the app actually runs in non-interactive Rscript contexts.
  shiny::runApp(app, launch.browser = TRUE)
}

if (interactive() || identical(environmentName(.GlobalEnv), "R_GlobalEnv")) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 1) {
    orient_mri_app(imagedir = args[1])
  }
}
