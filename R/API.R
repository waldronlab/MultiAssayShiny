.cleanC <- function(x) gsub(".__C__", "", x)
.cleanT <- function(x) gsub(".__T__", "", x)
#' Refer to the API documentation
#'
#' \code{API} opens a browser to the API documentation
#'
#' @param website (logical default TRUE) launch the API website
#' @param shiny (logical default FALSE) whether to launch the shiny version
#' of the API (experimental)
#'
#' @return Documentation via the GitHub wiki
#'
#' @examples
#' ## Runnable example does nothing
#'
#' API(website = FALSE)
#'
#' @author Vincent J Carey
#'
#' @import MultiAssayExperiment shinydashboard shiny
#' @importFrom utils methods
#' @importFrom methods extends
#'
#' @export API
API <- function(website = TRUE, shiny = FALSE) {
    if (shiny) {
        .apiDash()
    } else if (website) {
        utils::browseURL(
"https://github.com/WaldronLab/MultiAssayExperiment/wiki/MultiAssayExperiment-API"
        )
    } else {
        invisible()
    }
}

.packageAPI <- function(packname = "MultiAssayExperiment") {
    ns <- getNamespace(packname)
    nsElements <- ls(ns, all.names = TRUE)
    nsNS <- get(".__NAMESPACE__.", ns) # probably volatile?
    classes <- .cleanC(grep(".__C__", nsElements, value = TRUE))
    todrop <- sapply(classes, function(x) extends(x, "language"))
    if (any(todrop)) classes <- classes[-which(todrop)]
    TmethsWithin <- .cleanT(grep(packname, grep(".__T__", nsElements,
                                               value = TRUE),
                                value = TRUE))
    TmethsWithout <- .cleanT(grep(packname, grep(".__T__", nsElements,
                                                value = TRUE),
                                 value = TRUE, invert = TRUE))
    Mcmeths <- lapply(classes, function(x) utils::methods(class = x))
    names(Mcmeths) <- classes
    # list(nsElements = nsElements, nsNS = nsNS, classes = classes,
    # cmeths = cmeths)
    list(classes = classes, Mcmeths = Mcmeths, TmethsWithin = TmethsWithin,
         TmethsWithout = TmethsWithout)
}

.apiDash <- function() {
  ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
      textInput("packname", "Package", "MultiAssayExperiment")
    ),
    dashboardBody(
      # Boxes need to be put in a row (or column)
      #    fluidRow(
      #      box(plotOutput("plot1", height = 250)),
      #
      #      box(
      #        title = "Controls",
      #        sliderInput("slider", "Number of observations:", 1, 100, 50)
      #      )
      #    )
      fluidRow(
        tabBox(
          title = "API explorer",
          # The id lets us use input$tabset1 on the server to
          # find the current tab
          id = "tabset1", height = "250px",
          tabPanel("Classes",
                   tableOutput("ls1")),
          tabPanel("Methods(inside)", tableOutput("ls2")),
          tabPanel("Methods(outside)", tableOutput("ls3")),
          width = 9
        )
      )
    )
  )

  server <- function(input, output) {

    getconts <- reactive( {
      papi <- .packageAPI( input$packname )
      list(classDF = data.frame(classes = papi$classes),
           methsInDF = data.frame(methsIn = papi$TmethsWithin),
           methsOutDF = data.frame(methsOut = papi$TmethsWithout))
    })
    output$ls1 <- renderTable( getconts()$classDF )
    output$ls2 <- renderTable( getconts()$methsInDF )
    output$ls3 <- renderTable( getconts()$methsOutDF )
  }

  shinyApp(ui, server)
}
