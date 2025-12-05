#' @title update_xnat_images
#' @description Launches a Shiny UI to select a slice of the MRI image in the case that the disclosure
#' produces a blurry or distorted image
#' @param id The patient id as represented in the study database
#' @param outputdir The path name of the directory containing the participant feedback reports
#' @param ... Additional arguments passed to shiny::shinyApp
#' @return A Shiny app launched in the local browser to adjust the MRI images with a save button
#' @details Launches a Shiny UI to select a slice of the MRI image in the case that the disclosure
#' produces a blurry or distorted image
#' @seealso
#'  \code{\link[shiny]{fluidPage}}, \code{\link[shiny]{titlePanel}}, \code{\link[shiny]{sidebarLayout}}, \code{\link[shiny]{reexports}}, \code{\link[shiny]{radioButtons}}, \code{\link[shiny]{htmlOutput}}, \code{\link[shiny]{numericInput}}, \code{\link[shiny]{actionButton}}, \code{\link[shiny]{plotOutput}}, \code{\link[shiny]{textOutput}}, \code{\link[shiny]{renderUI}}, \code{\link[shiny]{renderPlot}}, \code{\link[shiny]{req}}, \code{\link[shiny]{observeEvent}}, \code{\link[shiny]{showNotification}}, \code{\link[shiny]{shinyApp}}
#' @rdname update_xnat_images
#' @export
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel h4 radioButtons uiOutput numericInput actionButton mainPanel plotOutput textOutput renderUI renderPlot req observeEvent showNotification shinyApp
#' @importFrom kuadrc.xnat read_nifti plot_nifti save_nifti_image

update_xnat_images <- function(id, outputdir, ...) {
  ui <- shiny::fluidPage(
    shiny::titlePanel("BRIDGE21: A Researcher Interface to Update MRI Images"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("Update MRI Images"),
        shiny::radioButtons(
          "plane",
          label = "Plane",
          choices = c("sagittal", "axial"),
          selected = "sagittal",
          inline = TRUE
        ),
        shiny::uiOutput("indexUI"),
        shiny::numericInput(
          "brightness",
          label = "Adjust Brightness",
          value = 1,
          min = 0,
          max = 3.0,
          step = 0.1
        ),
        shiny::actionButton("save", label = "Save Image")
      ),
      shiny::mainPanel(
        shiny::plotOutput("plot"),
        shiny::textOutput("message")
      )
    )
  )

  server <- function(input, output) {
    persondir <- file.path(outputdir, id)
    imagedir <- file.path(persondir, "imaging")

    nifti_file <- list.files(
      imagedir,
      pattern = ".nii.gz$",
      full.names = TRUE,
      recursive = TRUE
    )

    png_files <- list.files(
      imagedir,
      pattern = ".png$",
      full.names = TRUE,
      recursive = TRUE
    )

    scandir <- strsplit(nifti_file, split = "/nifti/")[[1]][1]

    axialImage <- png_files[grep("axial.png$", basename(png_files))]

    sagittalImage <- png_files[grep("sagittal.png$", basename(png_files))]

    nifti_data <- kuadrc.xnat::read_nifti(nifti_file)

    output$indexUI <- shiny::renderUI({
      shiny::numericInput(
        "index",
        label = "Change Index",
        value = which.max(apply(nifti_data, 3, sum)),
        min = 0,
        max = nrow(nifti_data),
        step = 5
      )
    })

    output$plot <- shiny::renderPlot({
      shiny::req(input$plane, input$index, input$brightness)
      kuadrc.xnat::plot_nifti(
        nifti_data,
        plane = input$plane,
        index = input$index,
        adjust_brightness = input$brightness
      )
    })

    shiny::observeEvent(input$save, {
      currentPlane <- input$plane
      image2save <- ifelse(input$plane == "sagittal", sagittalImage, axialImage)
      fileNameAs <- sprintf("%s_%s.png", id, currentPlane)
      kuadrc.xnat::save_nifti_image(
        path = file.path(scandir, "nifti"),
        plane = input$plane,
        index = input$index,
        adjust_brightness = input$brightness,
        save_file_as = fileNameAs
      )
      message <- paste(
        currentPlane,
        "image saved as",
        basename(image2save)
      )
      shiny::showNotification(message, type = "message")
    })
  }

  shiny::shinyApp(ui, server, ...)
}
