#' aajblay
#'
#' @description
#' JJHJJH!
#'
#' \strong{MODEL:}
#' !Prospect Theory (Sokol-Hessner et al., 2009, PNAS)
#'
#' @details
#' This section describes some of the function arguments in greater detail.
#'
#' \strong{data} should be assigned a character value specifying the full path and name of the file, including the file extension
#' (e.g. ".txt"), that contains the behavioral data of all subjects of interest for the current analysis.
#' The file should be a \strong{tab-delimited} text (.txt) file whose rows represent trial-by-trial observations and columns
#' \strong{*}Note: The data.txt file may contain other columns of data (e.g. "Reaction_Time", "trial_number", etc.), but only the data with the column
#' names listed above will be used for analysis/modeling. As long as the columns above are present and labelled correctly,
#' there is no need to remove other miscellaneous data columns.
#'
#' \strong{nwarmup} is a numerical value that specifies how many MCMC samples should not be stored upon the
#' beginning of each chain. For those familiar with Bayesian methods, this value is equivalent to a burn-in sample.
#' Due to the nature of MCMC sampling, initial values (where the sampling chain begins) can have a heavy influence
#' on the generated posterior distributions. The \code{nwarmup} argument can be set to a high number in order to curb the
#' effects that initial values have on the resulting posteriors.
#'
#' \strong{nchain} is a numerical value that specifies how many chains (i.e. independent sampling sequences) should be
#' used to draw samples from the posterior distribution. Since the posteriors are generated from a sampling
#' process, it is good practice to run multiple chains to ensure that a representative posterior is attained. When
#' sampling is completed, the multiple chains may be checked for convergence with the \code{plot(myModel, type = "trace")}
#' command. The chains should resemble a "furry caterpillar".
#'
#' \strong{nthin} is a numerical value that specifies the "skipping" behavior of the MCMC samples being chosen
#' to generate the posterior distributions. By default, \code{nthin} is equal to 1, hence every sample is used to
#' generate the posterior.
#'
#' \strong{Contol Parameters:} adapt_delta, stepsize, and max_treedepth are advanced options that give the user more control
#' over Stan's MCMC sampler. The Stan creators recommend that only advanced users change the default values, as alterations
#' can profoundly change the sampler's behavior. Refer to Hoffman & Gelman (2014, Journal of Machine Learning Research) for
#' more information on the functioning of the sampler control parameters. One can also refer to section 58.2 of the
#' \href{http://m...content-available-to-author-only...n.org/documentation/}{Stan User's Manual} for a less technical description of these arguments.
#'
#' @return
#'
#' @importFrom rstan vb sampling stan_model rstan_options extract
#' @importFrom parallel detectCores
#' @importFrom stats median qnorm density
#' @importFrom utils read.table
#'
#' @name hBayesDMgui
#'
#' @export
#'

hBayesDMgui <- function(){
  library(shiny)
  #File Name
  rm(list=ls())
  src_dir <- c("~/Documents/Github/hBayesDM/exec")
  src_file <- list.files(src_dir)
  a0 <- sapply(strsplit(src_file,split='\\.'),'[',1)
  a1 <- a0[grep("_",a0)]
  a2 <- sub('_','#',a1)
  f1 <- vapply(strsplit(a2,split='#'),'[',1,FUN.VALUE = character(1))

  # Define UI ----
  ui <- fluidPage(

    # App title ----
    titlePanel("SSHS OJBLAY Shiny!"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(

        # Input: Experiment
        selectInput("experiment",
                    h3("Experiment"),
                    choices = f1,
                    selected = 1),

        # Input: Model
        uiOutput('model'),

        # Input: Individual Check
        checkboxInput("individual", "individual", value = TRUE),

        # Input: Type
        uiOutput('type'),

        # Input: Slider for numbers
        sliderInput(inputId = "iter",
                    label = "Number of iter:",
                    min = 10,
                    max = 5000,
                    value = 2000),
        sliderInput(inputId = "warmup",
                    label = "Number of warmup:",
                    min = 5,
                    max = 2000,
                    value = 600),
        sliderInput(inputId = "chain",
                    label = "Number of chain:",
                    min = 1,
                    max = 10,
                    value = 4),
        sliderInput(inputId = "core",
                    label = "Number of core:",
                    min = 1,
                    max = 5,
                    value = 1),
        # Input: Go
        actionButton("go", "GO")
      ),

      # Main panel ----
      mainPanel(

        textOutput("selected_file"),
        textOutput("file"),
        p("For an introduction of models , read the ",
          a("Description",
            href = "http://rstudio-pubs-static.s3.amazonaws.com/164729_b8c91397d2d648cd928734286053e905.html#list-of-tasks-and-computational-models-implemented-in-hbayesdm")),       plotOutput(("result")),
        plotOutput(("progress"))

      )
    )
  )

  # Define server logic ----
  server <- function(input, output) {

    # RenderUI: Input: Model
    output$model = renderUI({
      exp<-paste("\\b",input$experiment,"#",sep="")
      selectInput("model",
                  "Model",
                  choices = vapply(strsplit(a2[grep(exp,a2)],split='#'),'[',2,FUN.VALUE = character(1)),
                  selected = 1)
    })

    # RenderUI: Input: Model
    output$type = renderUI({
      if(input$individual){
        typeChoice = NULL
      } else{
        typeChoice = c("dist", "trace", "simple")
      }
      selectInput("type",
                  "Type",
                  choices = typeChoice,
                  selected = 1)
    })

    # Output: Selected File
    output$selected_file <- renderText({
      paste("You have selected", input$experiment, "_", input$model)
    })

    # Output: Plot

    loadCode = eventReactive(input$go, {
      # Load Code
      paste0(input$experiment,'_',input$model)
    })

    inputFunction = eventReactive(input$go, {
      # Function <- Input
      list(
        data = paste0('~/Documents/Github/hBayesDM/inst/extdata/', input$experiment, '_', 'exampleData.txt'),
        niter=input$iter,
        nwarmup=input$warmup,
        nchain=input$chain,
        ncore=input$core
      )
    })

    ind = eventReactive(input$go, input$individual)
    typ = eventReactive(input$go, input$type)

    output$result <- renderPlot({
      # Run
      outputPlot<-do.call(

        # Load Code
        loadCode(),

        # Function <- Input
        inputFunction()
      )
      if(ind()){
        plotInd(outputPlot)
      }else{
        plot(outputPlot, type = typ())
      }
    })

    observeEvent(input$go, {
      output$progress <- renderPlot({
        if(input$go){ # Re-run when button is clicked
          withProgress(message = 'Making plot', value = 0, {
            # Number of times we'll go through the loop
            n <- 10

            for (i in 1:n) {
              # Increment the progress bar, and update the detail text.
              incProgress(1/n, detail = paste("Doing part", i))

              # Pause for 0.1 seconds to simulate a long computation.
              Sys.sleep(0.1)
            }
          })
        }
      })
    })

  }

  # Run the app ----
  shinyApp(ui = ui, server = server)
}
