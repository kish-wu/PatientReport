#' @title adjust_mri_images
#' @description Launches a Shiny UI to select a slice of the MRI image in the case that the disclosure
#' produces a blurry or distorted image. This version uses local utility functions.
#' @param id The patient id as represented in the study database
#' @param imagedir The path name of the directory containing the NIfTI and PNG images.
#' @param ... Additional arguments passed to shiny::shinyApp
#' @return A Shiny app launched in the local browser to adjust the MRI images with a save button.
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel h4 radioButtons uiOutput numericInput actionButton mainPanel plotOutput textOutput renderUI renderPlot req observeEvent showNotification shinyApp
#' @importFrom RNifti readNifti

# --- Local NIfTI Helper Functions ---

#' @title read_nifti_local
#' @description Reads a NIfTI file from a given path.
read_nifti_local <- function(nifti_file) {
  if (!file.exists(nifti_file)) {
    stop("NIfTI file not found at the specified path.")
  }
  RNifti::readNifti(nifti_file)
}

#' @title plot_nifti_local
#' @description Plots a slice of a NIfTI image.
plot_nifti_local <- function(nifti_data, plane = "sagittal", index = NULL, adjust_brightness = 1.0) {
  dims <- dim(nifti_data)
  if (is.null(index)) {
    index <- floor(dims[ifelse(plane == "sagittal", 1, 3)] / 2)
  }

  slice_data <- switch(plane,
    "sagittal" = nifti_data[index, , ],
    "axial" = nifti_data[, , index],
    stop("Invalid plane specified.")
  )

  slice_data <- slice_data * adjust_brightness
  par(mar = c(0, 0, 0, 0), bg = "black")
  image(t(slice_data), col = grDevices::gray(0:255 / 255), axes = FALSE)
}

#' @title save_nifti_image_local
#' @description Saves a specific slice of a NIfTI file as a PNG.
save_nifti_image_local <- function(nifti_file, plane, index, adjust_brightness, save_file_as) {
    nifti_data <- read_nifti_local(nifti_file)
    out_dir <- dirname(save_file_as)
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE)
    }
    png(filename = save_file_as, width = 600, height = 600, bg = "black")
    plot_nifti_local(nifti_data, plane = plane, index = index, adjust_brightness = adjust_brightness)
    dev.off()
}

# --- Main Shiny Application Function ---

adjust_mri_images <- function(id, imagedir, ...) {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Update MRI Images"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("Controls"),
        shiny::radioButtons("plane", "Plane", choices = c("sagittal", "axial"), selected = "sagittal", inline = TRUE),
        shiny::uiOutput("indexUI"),
        shiny::numericInput("brightness", "Brightness", value = 1, min = 0, max = 3.0, step = 0.1),
        shiny::actionButton("save", "Save Image")
      ),
      shiny::mainPanel(
        shiny::plotOutput("plot"),
        shiny::textOutput("message")
      )
    )
  )

  server <- function(input, output) {
    nifti_file <- list.files(imagedir, pattern = "\\.nii\\.gz$", full.names = TRUE, recursive = TRUE)

    if (length(nifti_file) == 0) {
        output$plot <- shiny::renderPlot({
            plot(1, type="n", axes=FALSE, xlab="", ylab="")
            text(1, 1, "Error: No .nii.gz file found in the image directory.", cex=1.5)
        })
        return()
    }
    nifti_file <- nifti_file[1]

    nifti_data <- read_nifti_local(nifti_file)

    output$indexUI <- shiny::renderUI({
      max_index <- dim(nifti_data)[ifelse(input$plane == "sagittal", 1, 3)]
      shiny::numericInput(
        "index",
        "Slice Index",
        value = floor(max_index / 2),
        min = 1,
        max = max_index,
        step = 1
      )
    })

    output$plot <- shiny::renderPlot({
      shiny::req(input$plane, input$index, input$brightness)
      plot_nifti_local(
        nifti_data,
        plane = input$plane,
        index = input$index,
        adjust_brightness = input$brightness
      )
    })

    shiny::observeEvent(input$save, {
      fileNameAs <- file.path(imagedir, sprintf("%s_%s.png", id, input$plane))

      save_nifti_image_local(
        nifti_file = nifti_file,
        plane = input$plane,
        index = input$index,
        adjust_brightness = input$brightness,
        save_file_as = fileNameAs
      )
      message <- paste(input$plane, "image saved as", basename(fileNameAs))
      shiny::showNotification(message, type = "message")
      shiny::stopApp() # return control to the caller once saved
    })
  }

  app <- shiny::shinyApp(ui, server, ...)
  # In non-interactive Rscript, shinyApp() just returns; runApp() ensures the UI actually launches.
  shiny::runApp(app, launch.browser = TRUE)
}
