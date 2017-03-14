

library(DT)

fieldsMandatory <- "title"

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    titlePanel("Saving report in separate files"),

    div(
      id = "form",

      #textInput("title", labelMandatory("Name"), ""),
      shiny::fluidRow(shiny::column(
        8,
        shiny::textAreaInput('title', 'Title', width = "900px", value = 'Title: Identify the report as a systematic review, meta-analysis, or both'))),
             shiny::fluidRow(shiny::column(
        8,
        shiny::textAreaInput(
          'abstract',
          'Abstract',
          rows = 4,
          width = "900px",
          value = 'Structured summary: Provide a structured summary including, as applicable: background; objectives; data sources; study eligibility criteria, participants, and interventions; study appraisal and synthesis methods; results; limitations; conclusions and implications of key findings; systematic review registration number.'
        )


      )),
      actionButton("submit", "Submit", class = "btn-primary")
    ),shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Thanks, your report was submitted successfully!"),
        actionLink("submit_another", "Submit another report")
      )
    )
  ) ,
  server = function(input, output, session) {
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)

      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    fieldsAll <- c("title", "abstract")
    responsesDir <- file.path("Responses")
    epochTime <- function() {
      as.integer(Sys.time())
    }
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })
    saveData <- function(data) {
      fileName <- sprintf("%s_%s.Rnw",
                          humanTime(),
                          digest::digest(data))

      write.csv(x = data, file = file.path(responsesDir, fileName),
                row.names = FALSE, quote = TRUE)
    }

    # action to take when submit button is pressed
    observeEvent(input$submit, {
      saveData(formData())
    })
    humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

    observeEvent(input$submit, {
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    })

    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })

  }
)

