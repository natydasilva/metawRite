#' Meta-analysis shiny app
#'
#' @usage metaupdate(datapair, pair_result,trt.pair, treat1, treat2, id)
#' @param datapair Data frame with treatment information for the pairwise meta-analysis (treat1 and treat2), id to identify each observation
#' and trt.pair with the string name for the pairwise comparison in alphabetic order, generated using pairwise_metafor in data folder
#' @param pair_result  list with the pairwise meta-analysis models generated using pairwise_metafor in data folder
#' @param trt.pair variable name with the pairwise treatment names
#' @param treat1 variable name with the treatment 1 in datapair
#' @param treat2 variable name with the treatment 2 in datapair
#' @param id variable with id information in datapair
#' @return shiny app.
#' @importFrom magrittr %>%
#' @export
# @examples
# metaupdate(MTCpairs2, pair_result, trt.pair, treat1, treat2, id)
metaupdate <- function(datapair, pair_result, trt.pair, treat1, treat2, id){
  library(plotly)
  ui = shiny::fluidPage(
    theme = "bootstrap.css",
    shiny::titlePanel("Meta-analysis app"),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel(
          "Publication",
          shiny::titlePanel("PRISMA Checklist"),
          shiny::fluidRow(shiny::column(
            width = 4,
            shiny::a(href = "http://www.sciencedirect.com/science/article/pii/S0167587712004023", "Version 1 June 2013", target =
                       "_blank")
          )),
          shiny::fluidRow(shiny::column(
            width = 4,
            shiny::a(
              href = "http://www.sciencedirect.com/science/article/pii/S016758771630191X",
              "Version 2 September 2016",
              target = "_blank"
            )
          )),

          shiny::fluidRow(shiny::column(
            8,
            shiny::textAreaInput('title', 'Title', width = "900px", value = 'Title: Identify the report as a systematic review, meta-analysis, or both')
          )) ,

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

          shiny::fluidRow(shiny::column(
            8,
            shiny::textAreaInput(
              'introduction',
              'Introduction',
              rows = 4,
              width = "900px",
              value = 'Rationale: Describe the rationale for the review in the context of what is already known. Objectives: Provide an explicit statement of questions being addressed with reference to participants, interventions, comparisons, outcomes, and study design (PICOS).'
            )

          )),
          shiny::fluidRow(shiny::column(
            8,
            shiny::textAreaInput(
              'method',
              'Methods',
              rows = 15,
              width = "900px",
              value = '
              Protocol and registration: Indicate if a review protocol exists, if and where it can be accessed (e.g., Web address), and, if available, provide registration information including registration number.
              Eligibility criteria: Specify study characteristics (e.g., PICOS, length of follow-up) and report characteristics (e.g., years considered, language, publication status) used as criteria for eligibility, giving rationale.
              Information sources: Describe all information sources (e.g., databases with dates of coverage, contact with study authors to identify additional studies) in the search and date last searched.
              Search: Present full electronic search strategy for at least one database, including any limits used, such that it could be repeated.
              Study selection: State the process for selecting studies (i.e., screening, eligibility, included in systematic review, and, if applicable, included in the meta-analysis).
              Data collection process: Describe method of data extraction from reports (e.g., piloted forms, independently, in duplicate) and any processes for obtaining and confirming data from investigators.
              Data items: List and define all variables for which data were sought (e.g., PICOS, funding sources) and any assumptions and simplifications made.
              Risk of bias in individual studies: Describe methods used for assessing risk of bias of individual studies (including specification of whether this was done at the study or outcome level), and how this information is to be used in any data synthesis.
              Summary measures:
              Synthesis of results: Describe the methods of handling data and combining results of studies, if done, including measures of consistency (e.g., I2) for each meta-analysis.
              Risk of bias across studies:Specify any assessment of risk of bias that may affect the cumulative evidence (e.g., publication bias, selective reporting within studies).
              Additional analyses:Describe methods of additional analyses (e.g., sensitivity or subgroup analyses, meta-regression), if done, indicating which were pre-specified.
              '
            )
            )),

          shiny::fluidRow(shiny::column(
            8,
            shiny::textAreaInput(
              'result',
              'Results',
              rows = 7,
              width = "900px",
              value = '
              Study selection: Give numbers of studies screened, assessed for eligibility, and included in the review, with reasons for exclusions at each stage, ideally with a flow diagram.
              Study characteristics: For each study, present characteristics for which data were extracted (e.g., study size, PICOS, follow-up period) and provide the citations.
              Risk of bias within studies: Present data on risk of bias of each study and, if available, any outcome level assessment (see item 12).
              Results of individual studies: For all outcomes considered (benefits or harms), present, for each study: (a) simple summary data for each intervention group (b) effect estimates and confidence intervals, ideally with a forest plot.
              Synthesis of results: Present results of each meta-analysis done, including confidence intervals and measures of consistency.
              Risk of bias across studies: Present results of any assessment of risk of bias across studies (see Item 15).
              Additional analysis:Give results of additional analyses, if done (e.g., sensitivity or subgroup analyses, meta-regression [see Item 16]).
              '
            )
            )),

          shiny::fluidRow(shiny::column(
            8,
            shiny::textAreaInput(
              'discussion',
              'Discussion',
              rows = 4,
              width = "900px",
              value = '
              Summary of evidence: Summarize the main findings including the strength of evidence for each main outcome; consider their relevance to key groups (e.g., healthcare providers, users, and policy makers).
              Limitations: Discuss limitations at study and outcome level (e.g., risk of bias), and at review-level (e.g., incomplete retrieval of identified research, reporting bias).
              Conclusions: Provide a general interpretation of the results in the context of other evidence, and implications for future research.
              '
            )
            )),
          shiny::fluidRow(
            shiny::column(
              8,
              shiny::textAreaInput(
                'funding',
                'Funding',
                rows = 2,
                width = "900px",
                value = 'Describe sources of funding for the systematic review and other support (e.g., supply of data); role of funders for the systematic review.
                '
              ),
              shiny::tags$style(
                type = 'text/css',
                "#introduction { height: 100px; width: 100%; display: block;}"
              )
              )
            ),
          shiny::fluidRow(align = "center", shiny::downloadButton('report'))
          ),


        shiny::tabPanel(
          "Pairwise",
          shiny::fluidRow(shiny::column(
            width = 3,
            shiny::a(href = "http://www.metafor-project.org/doku.php/metafor", "metafor package help", target =
                       "_blank")
          )),
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::numericInput(
                'update',
                'Update:',
                1,
                min = 1,
                max = length(pair_result)
              )
            ),
            shiny::column(
              width = 6,
              shiny::selectInput(
                'pair',
                'Pairwise treatment:',
                unique(datapair$trt.pair),
                selected =  unique(datapair$trt.pair)[1]
                #datapair%>%filter(up==input$update)%>%select(trt.pair) %>%unique(),
                #selected = datapair%>%filter(up==input$update)%>%select(trt.pair) %>%unique()[1]
              )
            )
          ),
          shiny::fluidRow(shiny::column(
            width =  6, shiny::plotOutput("forest")
          ),shiny::column(
            width =  6, shiny::plotOutput("funel")
          )
          # ,shiny::column(
          #   width =  6, shiny::plotOutput("labbe")
          # )
          ),
          shiny::fluidRow(shiny::column(
            width =  10, shiny::verbatimTextOutput("summary")
          )),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              align = "center",
              shiny::tableOutput('tablerank')
            )
          )
        ),

        shiny::tabPanel(
          "Network" ,


          shiny::fluidRow(shiny::column(width = 6,
                                     plotly::plotlyOutput("netply"))),
          shiny::fluidRow(shiny::column(width = 12 ,shiny::verbatimTextOutput("click")))


        )
            )
          )
      )



  server = function(input, output) {
    output$report = shiny::downloadHandler(
      filename = 'myreport.pdf',

      content = function(file) {
        tempReport <- file.path(tempdir(), "inst/input.Rnw")
        file.copy("inst/input.Rnw", tempReport, overwrite = TRUE)

        writeLines(input$title, con = "inst/_title.Rnw")
        writeLines(input$abstract, con = "inst/_abstract.Rnw")
        writeLines(input$introduction, con = "inst/_introduction.Rnw")
        writeLines(input$method, con = "inst/_methods.Rnw")
        writeLines(input$result, con = "inst/_results.Rnw")
        writeLines(input$discussion, con = "inst/_discussion.Rnw")
        writeLines(input$funding, con = "inst/_funding.Rnw")
        out = knitr::knit2pdf(input='inst/input.Rnw', output="inst/input.tex", clean = TRUE)
        file.rename(out, file) # move pdf to file for downloading
      },

      contentType = 'inst/application/pdf'
    )

    output$forest <- shiny::renderPlot({
      pair <- names(pair_result[[input$update]]) %in% input$pair
      npair <- 1:length(pair)
      metafor::forest(pair_result[[input$update]][[npair[pair]]][[2]])

      })

    output$funel <- shiny::renderPlot({
      pair <- names(pair_result[[input$update]]) %in% input$pair
      npair <- 1:length(pair)
      metafor::funnel(pair_result[[input$update]][[npair[pair]]][[2]])

    })

    output$labbe <- shiny::renderPlot({
      pair <- names(pair_result[[input$update]]) %in% input$pair
      npair <- 1:length(pair)
      metafor::labbe(pair_result[[input$update]][[npair[pair]]][[2]])

    })
    output$summary <- shiny::renderPrint({
      pair <- names(pair_result[[input$update]]) %in% input$pair

      npair <- 1:length(pair)

      return(print(pair_result[[input$update]][[npair[pair]]][[2]]))
    })

    rv <- shiny::reactiveValues(data = data.frame(datapair, fill = logical(length(datapair$id))))

    output$netply <- plotly::renderPlotly({
      p <-
        ggplot2::ggplot(data = datapair, ggplot2::aes(
          from_id = treat1,
          to_id = treat2,
          key = id
        ))

      p2 <-
        p + geomnet::geom_net(
          layout.alg = "circle",
          size = 3,
          ggplot2::aes(col = treat1, key = datapair$id),
          labelon = TRUE
        ) +
        geomnet::theme_net() + ggplot2::theme(legend.position = "none") + ggplot2::scale_colour_brewer(palette = "Set3")

      plotly::ggplotly(p2) %>% plotly::layout(dragmode = "select")

    })

    output$click <- shiny::renderPrint({
      d <- plotly::event_data("plotly_click")
      if (is.null(d)) {
        "Click events appear here (double-click to clear)"
      } else{
        d
      }
    })

  }

  shiny::shinyApp(ui, server )

}
