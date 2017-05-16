
#' Meta-analysis reportshiny app, tab 1 draft version to persistent local storage
#'
#' @usage upreport(datapair, pair_result,trt.pair, treat1, treat2, id)
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
#' @examples
#'\dontrun{load("./data/MTCdata.rda")
#' load("./data/dat_rungano.rda")
#' MTCpairs <- netmeta::pairwise(list(treat1, treat2, treat3),
#'                 list(event1, event2, event3),
#'               list(n1, n2, n3),
#'                 data = MTCdata,
#'                 sm = "RR")
#' MTCpairsrg <- netmeta::pairwise(list(t1, t2, t3, t4),
#'                 TE = list(y1, y2, y3, y4),
#'               seTE = list(se1, se2, se3, se4),
#'                 data = dat_rungano,
#'                 sm = "MD")
#' modstr <- pairwise_metafor(MTCpairs, nupdate = 2, treat1 = treat1, 
#' treat2 = treat2, nobs = c(109, 5), method  = "REML", measure = "RR")
#' 
#' modstr2 <- pairwise_metafor(MTCpairsrg, nupdate = 1, treat1 = treat1, 
#' treat2 = treat2, nobs = 29, method  = "REML", measure = "GEN")
#' upreport(modstr[[1]],  modstr[[2]], trt.pair, treat1, treat2, id)
#' upreport(modstr2[[1]], modstr2[[2]], trt.pair, treat1, treat2, id)
#' 
#' }
#' 
#' 
upreport <-
  function(datapair,
           pair_result,
           trt.pair,
           treat1,
           treat2,
           id) {
    lsr <- list(title='Title: Identify the report as a systematic review, meta-analysis, or both',
                abstract = "Structured summary: Provide a structured summary including, as applicable: background; objectives; data sources; study eligibility criteria, participants, and interventions;study appraisal and synthesis methods; results; limitations; conclusions and implications of key findings; systematic review registration number.",
                introduction = "Rationale: Describe the rationale for the review in the context of what is already known. Objectives: Provide an explicit statement of questions being addressed with reference to participants, interventions, comparisons, outcomes, and study design (PICOS).",
                method = "Protocol and registration: Indicate if a review protocol exists, if and where it can be accessed (e.g., Web address), and, if available, provide registration information including registration number.
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
                Additional analyses:Describe methods of additional analyses (e.g., sensitivity or subgroup analyses, meta-regression), if done, indicating which were pre-specified.",
                result = " Study selection: Give numbers of studies screened, assessed for eligibility, and included in the review, with reasons for exclusions at each stage, ideally with a flow diagram.
                Study characteristics: For each study, present characteristics for which data were extracted (e.g., study size, PICOS, follow-up period) and provide the citations.
                Risk of bias within studies: Present data on risk of bias of each study and, if available, any outcome level assessment (see item 12).
                Results of individual studies: For all outcomes considered (benefits or harms), present, for each study: (a) simple summary data for each intervention group (b) effect estimates and confidence intervals, ideally with a forest plot.
                Synthesis of results: Present results of each meta-analysis done, including confidence intervals and measures of consistency.
                Risk of bias across studies: Present results of any assessment of risk of bias across studies (see Item 15).
                Additional analysis:Give results of additional analyses, if done (e.g., sensitivity or subgroup analyses, meta-regression [see Item 16]).",
                discussion = "   Summary of evidence: Summarize the main findings including the strength of evidence for each main outcome; consider their relevance to key groups (e.g., healthcare providers, users, and policy makers).
                Limitations: Discuss limitations at study and outcome level (e.g., risk of bias), and at review-level (e.g., incomplete retrieval of identified research, reporting bias).
                Conclusions: Provide a general interpretation of the results in the context of other evidence, and implications for future research.",
                funding = "Describe sources of funding for the systematic review and other support (e.g., supply of data); role of funders for the systematic review.
                ")
    
#
# library(plotly)

ui = shiny::fluidPage(
  shinyjs::useShinyjs(),
  #shinyjs::inlineCSS(appCSS),
  shiny::titlePanel("Review, write and update meta-analysis results"),
  shiny::mainPanel(
    shiny::tabsetPanel(
  shiny::tabPanel(
    "LSR-report",
  shinyjs::hidden(
    shiny::div(
      id = "reportupdate",
      #selectInput("update", "Update report",filenames)
      shiny::uiOutput("update")
    )
  ),

  shiny::div(
    id = "form",
    shiny::fluidRow(shiny::column(
      8,
      shiny::textAreaInput(
        'title',
        'Title',
        width = "900px",
        value = lsr$title, resize ="vertical")
      )),

    shiny::fluidRow(shiny::column(
      8,
      shiny::textAreaInput(
        'abstract',
        'Abstract',
        rows = 4,
        width = "900px",
        value = lsr$abstract, resize ="vertical")
    )),

    shiny::fluidRow(shiny::column(
      8,
      shiny::textAreaInput(
        'introduction',
        'Introduction',
        rows = 4,
        width = "900px",
        value = lsr$introduction,  resize ="vertical" )
    )),
    shiny::fluidRow(shiny::column(
      8,
      shiny::textAreaInput(
        'method',
        'Methods',
        rows = 8,
        width = "900px",
        value = lsr$method, resize ="vertical")
    )),

    shiny::fluidRow(shiny::column(
      8,
      shiny::textAreaInput(
        'result',
        'Results',
        rows = 8,
        width = "900px",
        value = lsr$result, resize ="vertical")
    )),

    shiny::fluidRow(shiny::column(
      8,
      shiny::textAreaInput(
        'discussion',
        'Discussion',
        rows = 4,
        width = "900px",
        value = lsr$discussion, resize ="vertical")
    )),
    shiny::fluidRow(
      shiny::column(
        8,
        shiny::textAreaInput(
          'funding',
          'Funding',
          rows = 2,
          width = "900px",
          value = lsr$funding, resize ="vertical"
        )
      )
    ),


    shiny::actionButton("submit", "Submit", class = "btn-primary"),
    shiny::downloadButton('download')
  ), shiny::fluidRow(shiny::column(
    8,
    shiny::HTML("<div style='height: 150px;'>"),

    shiny::HTML("</div>")
    )),
  shinyjs::hidden(
    shiny::div(
      id = "thankyou_msg",
      shiny::h3("Thanks, your report was submitted successfully!"),
      shiny::actionLink("submit_another", "Submit another report")
      )
    )
  ),  
  shiny::tabPanel(
    "Pairwise" ,
    shiny::fluidRow( shiny::column(6, shiny::selectInput("treatpair",
                                           "Pairwise comparison:", choices = datapair %>% dplyr::select(trt.pair) %>% unique() ) ), 
                     shiny::column(3, shiny::uiOutput("updt"))), 
                         shiny::actionButton("goButton2", "Initial selection!"),
    
  # shiny::tabPanel(
  #   "Pairwise" ,
  #   shiny::fluidRow(shiny:: numericInput("updatelab", "Update:",value = 1,   min = 1,
  #                                        max = length(pair_result)),
  #                   shiny::uiOutput("mytreat"), shiny::actionButton("goButton", "Initial selection!")),
    shiny::fluidRow(
      shiny::column(width =  6, shiny::plotOutput("forest2" ) ),
      shiny::column(width =  6, shiny::plotOutput("funel2" ) )

    ),
    shiny::fluidRow(shiny::column(
      width =  10, shiny::verbatimTextOutput("summary2")
    ))
  ),

  shiny::tabPanel(
    "Network" ,
    shiny::fluidRow(shiny::column(
      width = 6,
      plotly::plotlyOutput("netply")
    )),
    shiny::fluidRow(shiny::column(
      width = 12 , shiny::verbatimTextOutput("click")
    ))


  ),
  # shiny::tabPanel(
  #   "Paper search",
  #   shiny::fluidRow(shiny::column(
  #     width = 12,
  #     "In this tab we can include possible search for new papers, next update material")),
  #   shiny::fluidRow(shiny::textAreaInput(
  #     'search',
  #     'Search',
  #     rows = 2,
  #     width = "900px",
  #     value = "pinkeye", resize ="vertical"
  #   ) )
  # )
  shiny::tabPanel(
    "Paper search",
    shiny::sidebarLayout(
  shiny::sidebarPanel(
    shiny::helpText("Type a word below and search PubMed to find documents that contain that word in the text. You can even type multiple words. You can search authors, topics, any acronym, etc."),
    shiny::textInput("text", label = h3("Keyord(s)"), value = "pinkeye in cows"),
    shiny::helpText("You can specify the start and end dates of your search, use the format YYYY/MM/DD"),
    shiny::textInput("date1", label = h3("From"),value="2016/01/01"),
    shiny::textInput("date2", label = h3("To"),  value = "2017/01/01"),
    shiny::helpText("Now select the output you'd like to see. You can see a barplot of articles per year, a wordcloud of the abstract texts, or a table of the top six authors"),
    shiny::actionButton("wordButton","WORDS")),
  
  shiny::mainPanel(
    shiny::tableOutput("authList")
    
  )
  )))
))

server = function(input, output, session) {


  output$download = shiny::downloadHandler(
    #output$download <- observeEvent(input$download, {shiny::downloadHandler(
  filename = 'myreport.pdf',

    content = function(file) {

           # browser()
           # tmp <- tempdir()

      tmp <- system.file(package="metaupdate")
      tempReport <- file.path(tmp,"input2.Rnw")
      file.copy(file.path(tmp, "input.Rnw"), tempReport, overwrite = TRUE)
      dir <- system.file(package="metaupdate")


      writeLines(input$title, con = file.path(dir, "_title.Rnw"))
      writeLines(input$abstract, con = file.path(dir, "_abstract.Rnw"))
      writeLines(input$introduction, con = file.path(dir, "_introduction.Rnw"))
      writeLines(input$method, con = file.path(dir, "_methods.Rnw"))
      writeLines(input$result, con = file.path(dir, "_results.Rnw"))
      writeLines(input$discussion, con = file.path(dir, "_discussion.Rnw"))
      writeLines(input$funding, con = file.path(dir, "_funding.Rnw"))
      out = knitr::knit2pdf(input = tempReport,
                            output = file.path(tmp, "input.tex"),
                            clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
    }

  )

 # })


  responsesDir <- file.path("tools")

  #Dynamic UI with updated information of the files in tools folder
  # output$update <- shiny::renderUI({
  #  # reactiveFileReader(1000,)
  #   filenames <- sort(dir("tools"),TRUE)
  #   reportnames <- unique(substr(filenames, 1,15))
  #   shiny::selectInput("update", "Update report", reportnames)
  # })

  #Make reactive the new information in the report


  title <- shiny::reactive({
    list("title",input$title)
  })
    abstract <- shiny::reactive({
      list("abstract", input$abstract)
      })
    introduction <- shiny::reactive({
      list("introduction", input$introduction)
    })

    method <- shiny::reactive({
      list("method", input$method)
    })

    result <- shiny::reactive({

      list("result", input$result)
    })

    discussion <- shiny::reactive({
      list("discussion", input$discussion)
    })

    funding <- shiny::reactive({
      list("funding", input$funding)
    })

    Timereport <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
    ccaux <- list("title", "abstract", "introduction", "method", "result", "discussion", "funding")

    saveData <- function(data,cc) {
      if(length(data[[2]] > 0)){
        if(!file.exists("tools")) system(sprintf("mkdir %s", "tools"))

        fileName <- paste("tools/",Timereport(),data[[1]],".txt", sep="")
        fileConn <- file(fileName)
        writeLines(data[[2]], fileConn)
        close(fileConn)
      }else{
        fileName <- paste("tools/",Timereport(),cc,".txt", sep="")
        fileConn <- file(fileName)
        writeLines(input$noquote(cc), fileConn)
        close(fileConn)

      }

    }

  # action to take when submit button is pressed

  shiny::observeEvent(input$submit, {

    # Save the new information in  the report  in a txt with name = date and time
    saveData(title(), ccaux[[1]])
    saveData(abstract(), ccaux[[2]])
    saveData(introduction(), ccaux[[3]])
    saveData(method(), ccaux[[4]])
    saveData(result(), ccaux[[5]])
    saveData(discussion(), ccaux[[6]])
    saveData(funding(), ccaux[[7]])

    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")
  })


  # action to take when write new report in each textAreaInput

  shiny::observeEvent(input$update,{
    x <- input$update
    titlePath <- file.path(paste("tools/",x,ccaux[[1]],".txt", sep=""))
    abstractPath <- file.path(paste("tools/",x,ccaux[[2]],".txt", sep=""))
    introductionPath <- file.path(paste("tools/",x,ccaux[[3]],".txt", sep=""))
    methodPath <- file.path(paste("tools/",x,ccaux[[4]],".txt", sep=""))
    resultPath <- file.path(paste("tools/",x,ccaux[[5]],".txt", sep=""))
    discussionPath <- file.path(paste("tools/",x,ccaux[[6]],".txt", sep=""))
    fundingPath <- file.path(paste("tools/",x,ccaux[[7]],".txt", sep=""))

    titleUpdate <- readLines(titlePath)
    abstractUpdate <- readLines(abstractPath)
    introductionUpdate <- readLines(introductionPath)
    methodUpdate <- readLines(methodPath)
    resultUpdate <- readLines(resultPath)
    discussionUpdate <- readLines(discussionPath)
    fundingUpdate <- readLines(fundingPath)

    shiny::updateTextAreaInput(session, "title", value = titleUpdate)
    shiny::updateTextAreaInput(session, "abstract", value = abstractUpdate)
    shiny::updateTextAreaInput(session, "introduction", value = introductionUpdate)
    shiny::updateTextAreaInput(session, "method", value = methodUpdate)
    shiny::updateTextAreaInput(session, "result", value = resultUpdate)
    shiny::updateTextAreaInput(session, "discussion", value = discussionUpdate)
    shiny::updateTextAreaInput(session, "funding", value = fundingUpdate)

  })



  # action to take when a submit another button is pressed

  shiny::observeEvent(input$submit_another, {

    shinyjs::show("reportupdate")
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")

    output$update <- shiny::renderUI({
      # reactiveFileReader(1000,)
      filenames <- sort(dir("tools"),TRUE)
      reportnames <- unique(substr(filenames, 1,15))
      shiny::selectInput("update", "Update report", reportnames)
    })

  })


  ###############
  #   TAB 2     #
  ###############
# 
#   selectedData <- shiny::reactive({
#     input$goButton
# 
#   })

# 
#   up <- NULL
#   sel <- datapair %>% dplyr::filter(up%in% "1") %>% dplyr::select(trt.pair) %>% unique()
#   output$mytreat <- shiny::renderUI({
# 
#     choi <- datapair %>% dplyr::filter(up%in% input$updatelab) %>% dplyr::select(trt.pair) %>% unique()
#     #browser()
#     shiny::selectInput(
#       "treatpair",
#       "Pairwise comparison:", choices = choi
# 
#     )
#   })
# 
#   output$forest2 <- shiny::renderPlot({
# 
#     if(selectedData()){
# 
#       pardat <- pair_result[[as.numeric(input$updatelab)]]
#       pair <- names(pardat) %in% input$treatpair
#       npair <- 1:length(pair)
# 
#       metafor::forest(pardat[[npair[pair]]][[2]])
#     }else{
#       return(NULL)
#     }
#   })
# 
# 
# 
# 
# 
#   output$funel2 <- shiny::renderPlot({
# 
#     if(selectedData()){
#       pardat <- pair_result[[as.numeric(input$updatelab)]]
# 
#       pair <-
#         names(pardat) %in% input$treatpair
#       npair <- 1:length(pair)
#       metafor::funnel(pardat[[npair[pair]]][[2]])
#     }else{
#       return(NULL)
#     }
#   })
# 
# 
#   output$summary2 <- shiny::renderPrint({
#     if(selectedData()){
#       pardat <- pair_result[[as.numeric(input$updatelab)]]
# 
#       pair <-
#         names(pardat) %in% input$treatpair
# 
#       npair <- 1:length(pair)
# 
#       return(print(pardat[[npair[pair]]][[2]]))
#     }else{
#       return(NULL)
#     }
#   })
  
  up <- NULL
  sel <- datapair %>% dplyr::filter(up%in% "1") %>% dplyr::select(trt.pair) %>% unique()
  output$updt <- shiny::renderUI({
    
    choi <- datapair %>% dplyr::filter(trt.pair %in% input$treatpair)  %>%  dplyr::select(up) %>% unique()
    
    shiny:: numericInput("updatelab", "Update:",value = 1,   min = 1,
                         max = choi)
  })
  
  shiny::observeEvent(input$goButton2, {
  
  output$forest2 <- shiny::renderPlot({
   
    
    pardat <- shiny::isolate(pair_result %>% 
      dplyr::filter(trt.pair %in% input$treatpair))
    
 forest_metafor(pardat[[1, 'model']][[as.numeric(input$updatelab)]])
    
  })
  
  
  
  output$funel2 <- shiny::renderPlot({
    
    pardat <- shiny::isolate(pair_result %>%
      dplyr::filter(trt.pair %in% input$treatpair))
    
 metafor::funnel( pardat[[1, 'model']][[as.numeric(input$updatelab)]] )
    
    
  })
  
  output$summary2 <- shiny::renderPrint({
    pardat <- shiny::isolate(pair_result %>% 
      dplyr::filter( trt.pair %in% input$treatpair))
    
    return(print(pardat[[1, 'model']][[ as.numeric( input$updatelab )]]))
    
  })
  })

  ###############
  #   TAB 3     #
  ###############
  rv <-
    shiny::reactiveValues(data = data.frame(datapair, fill = logical(length(datapair$id))))

  output$netply <- plotly::renderPlotly({
    p <-
      ggplot2::ggplot(data = datapair,
                      ggplot2::aes(
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
      geomnet::theme_net() + ggplot2::theme(legend.position = "none")
    #+ ggplot2::scale_colour_brewer(palette = "Set3")

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




shiny::shinyApp(ui,server)

}
