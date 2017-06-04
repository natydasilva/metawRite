#' Meta-analysis reportshiny app, tab 1 draft version to persistent local storage
#'
#' @usage upreport(initialprotocol = TRUE, initialreport = TRUE, 
#' pair = FALSE, net = FALSE, data = NULL)
#' @param initialprotocol logic value to indicate if is the initial protocol, default is TRUE
#' @param initialreport logic value to indicate if is the initial review, default is TRUE
#' @param pair logic value to indicate if pairwise meta-analysis is available, default is FALSE
#' @param net logic value to indicate if the analysisi will include a network meta-analysis, default is FALSE
#' @param data list with two components, a data frame with treatment information for the pairwise meta-analysis (treat1 and treat2), id to identify each observation
#' and trt.pair with the string name for the pairwise comparison in alphabetic order, generated using pairwise_metafor in data folder. The second
#' element is a list with the pairwise meta-analysis models generated using pairwise_metafor in data folder
#' @return shiny app.
#' @importFrom magrittr %>%
#' @export
#'@examples
#'\dontrun{
#' 
#' upreport(initialprotocol = TRUE, initialreport = TRUE, 
#' pair = FALSE, net = FALSE, data = NULL)  
#' }
 
upreport <-
  function(initialprotocol = TRUE, initialreport =TRUE, pair = FALSE, net = FALSE, data = NULL) {
    
    if(is.null(data)){
      datapair <-NULL
      pair_result <- NULL
      trt.pair <- NULL
      treat1 <- NULL
      treat2 <- NULL
      id <- NULL
     }else{
      datapair <- data[[1]]
      pair_result <- data[[2]]
      trt.pair <- data[[1]]$trt.pair
      treat1 <- data[[1]]$treat1
      treat2 <- data[[1]]$treat2
      id <- data[[1]]$id
      
     
    }
    lsr <- list(title='Title: Identify the report as a systematic review, meta-analysis, or both',
                abstract = 'Structured summary: Provide a structured summary including, as applicable: background; objectives; data sources; study eligibility criteria, participants, and interventions;study appraisal and synthesis methods; results; limitations; conclusions and implications of key findings; systematic review registration number.',
                introduction = 'Rationale: Describe the rationale for the review in the context of what is already known. Objectives: Provide an explicit statement of questions being addressed with reference to participants, interventions, comparisons, outcomes, and study design (PICOS).',
                method = 'Protocol and registration: Indicate if a review protocol exists, if and where it can be accessed (e.g., Web address), and, if available, provide registration information including registration number.
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
                Additional analyses:Describe methods of additional analyses (e.g., sensitivity or subgroup analyses, meta-regression), if done, indicating which were pre-specified.',
                result = 'Study selection: Give numbers of studies screened, assessed for eligibility, and included in the review, with reasons for exclusions at each stage, ideally with a flow diagram.
                Study characteristics: For each study, present characteristics for which data were extracted (e.g., study size, PICOS, follow-up period) and provide the citations.
                Risk of bias within studies: Present data on risk of bias of each study and, if available, any outcome level assessment (see item 12).
                Results of individual studies: For all outcomes considered (benefits or harms), present, for each study: (a) simple summary data for each intervention group (b) effect estimates and confidence intervals, ideally with a forest plot.
                Synthesis of results: Present results of each meta-analysis done, including confidence intervals and measures of consistency.
                Risk of bias across studies: Present results of any assessment of risk of bias across studies (see Item 15).
                Additional analysis:Give results of additional analyses, if done (e.g., sensitivity or subgroup analyses, meta-regression [see Item 16]).',
                discussion = 'Summary of evidence: Summarize the main findings including the strength of evidence for each main outcome; consider their relevance to key groups (e.g., healthcare providers, users, and policy makers).
                Limitations: Discuss limitations at study and outcome level (e.g., risk of bias), and at review-level (e.g., incomplete retrieval of identified research, reporting bias).
                Conclusions: Provide a general interpretation of the results in the context of other evidence, and implications for future research.',
                funding = 'Describe sources of funding for the systematic review and other support (e.g., supply of data); role of funders for the systematic review.
                ')
    
    protocol <- list(titleproto ="Identification: Identify the report as a protocol of a systematic review,  update: if the protocol is for an update of a previous systematic review, identify as such,
                     contact: Provide name, institutional affiliation, e-mail address of all protocol authors; provide physical mailing address of corresponding author, contributions: Describe contributions of protocol authors and identify the guarantor of the review,
                     Amendments: If the protocol represents an amendment of a previously completed or published protocol, identify as such and list changes; otherwise, state plan for documenting important protocol amendments,
                     Support: Indicate sources of financial or other support for the review, Sponsors:Provide name for the review funder and/or sponsor,
                     Role of sponsor or funder: Describe roles of funder(s), sponsor(s), and/or institution(s), if any, in developing the protocol",
                     introproto ="Rationale:Describe the rationale for the review in the context of what is already known,
                     Objective: Provide an explicit statement of the question(s) the review will address with reference to participants, interventions, comparators, and outcomes (PICO)", 
                     methodproto = "Eligibility criterio: Specify the study characteristics (such as PICO, study design, setting, time frame) and report characteristics (such as years considered, language, publication status) to be used as criteria for eligibility for the review,
                     Information source: Describe all intended information sources (such as electronic databases, contact with study authors, trial registers or other grey literature sources) with planned dates of coverage,
                     Search strategy: Present draft of search strategy to be used for at least one electronic database, including planned limits, such that it could be repeated,
                     Study records: Describe the mechanism(s) that will be used to manage records and data throughout the review,
                     Selection process: State the process that will be used for selecting studies (such as two independent reviewers) through each phase of the review (that is, screening, eligibility and inclusion in meta-analysis),
                     Data collection process: Describe planned method of extracting data from reports (such as piloting forms, done independently, in duplicate), any processes for obtaining and confirming data from investigators,
                     Data items: List and define all variables for which data will be sought (such as PICO items, funding sources), any pre-planned data assumptions and simplifications,
                     Outcomes and priorization: List and define all outcomes for which data will be sought, including prioritization of main and additional outcomes, with rationale,
                     Risk of Bias: Describe anticipated methods for assessing risk of bias of individual studies, including whether this will be done at the outcome or study level, or both; state how this information will be used in data synthesis,
                     Data synthesis: Describe criteria under which study data will be quantitatively synthesised
                     If data are appropriate for quantitative synthesis, describe planned summary measures, methods of handling data and
                     methods of combining data from studies, including any planned exploration of consistency (such as I2, Kendalls T) 
                     Describe any proposed additional analyses (such as sensitivity or subgroup analyses, meta-regression)
                     If quantitative synthesis is not appropriate, describe the type of summary planned,
                     Meta-bias(es): Specify any planned assessment of meta-bias(es) (such as publication bias across studies, selective reporting within studies),
                     Confidence in cumulative evidence: Describe how the strength of the body of evidence will be assessed (such as GRADE)")
    #
    # library(plotly)
    
    
    ui = shiny::fluidPage(
      shinyjs::useShinyjs(),
      #shinyjs::inlineCSS(appCSS),
      shiny::titlePanel("Review, write and update meta-analysis results"),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Protocol",
            shinyjs::hidden(
              shiny::div(
                id = "updateproto",
                shiny::uiOutput("updateproto"))
            ),
            
            shiny::div(
              id = "formproto",
              shiny::fluidRow(shiny::column(
                8,
                shiny::textAreaInput(
                  'titleproto',
                  'Title',
                  rows = 4,
                  width = "900px",
                  value = protocol$titleproto, resize ="vertical")
              )),
              shiny::fluidRow(shiny::column(
                8,
                shiny::textAreaInput(
                  'introproto',
                  'Introduction',
                  rows = 4,
                  width = "900px",
                  value = protocol$introproto, resize ="vertical")
              )),
              shiny::fluidRow(shiny::column(
                8,
                shiny::textAreaInput(
                  'methodproto',
                  'Methods',
                  rows = 14,
                  width = "900px",
                  value = protocol$methodproto, resize ="vertical")
              )),
              
              
              shiny::actionButton("submitproto", "Submit protocol", class = "btn-primary"),
              shiny::downloadButton(outputId='downproto', label="Download .pdf")
             # shiny::downloadButton(outputId="downprotornw",label="Download .Rnw")
              ),
            shiny::fluidRow(shiny::column(8,
                                          shiny::HTML("<div style='height: 150px;'>"),
                                          
                                          shiny::HTML("</div>")
            )),
            shinyjs::hidden(
              shiny::div(
                id = "thankyou_msgproto",
                shiny::h3("Thanks, your protocol was submitted successfully!"),
                shiny::actionLink("submit_anotherproto", "Submit another report")
              )
            )),
          shiny::tabPanel(
            "PubMed",
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                shiny::helpText("Type a word below to search in PubMed, you can search authors, topics, any acronym, etc"),
                shiny::textInput("serchtext", label = shiny::h3("Keywords"), value = "pinkeye in cows"),
                shiny::helpText("Specify the start and end dates of your search, use the format YYYY/MM/DD"),
                shiny::textInput("date1", label = shiny::h3("From"),value="2014/01/01"),
                shiny::textInput("date2", label = shiny::h3("To"),  value = "2017/01/01"),
                shiny::helpText("Now select serch and you can see the paper title, authors and publication year"),
                shiny::actionButton("wordButton","Search")),
              
              shiny::mainPanel(
                shiny::HTML("<div style='height: 50px;'>"),
                shiny::HTML("</div>"),
                shiny::tableOutput("wordtext")
              ))),
          
          
          shiny::tabPanel(
            " PubAg",
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                shiny::helpText("Type a word below to search in PubAg, you can search ...."),
                shiny::textInput("serchtextag", label = shiny::h3("Keywords"), value = "pinkeye"),
                # shiny::helpText("Specify the start and end dates of your search, use the format YYYY/MM/DD"),
                # shiny::textInput("date1ag", label = shiny::h3("From"),value="2014/01/01"),
                # shiny::textInput("date2ag", label = shiny::h3("To"),  value = "2017/01/01"),
                # shiny::helpText("Now select serch and you can see the paper title, authors and publication year"),
                shiny::actionButton("wordButtonAg","Search")),
              
              shiny::mainPanel(
                shiny::HTML("<div style='height: 50px;'>"),
                shiny::HTML("</div>"),
                shiny::tableOutput("wordtextAg")
              ))),
          
          
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
            shinyjs::hidden(
              shiny::div(
                id = "pairupdate",
                shiny::fluidRow( shiny::column(6, shiny::selectInput("treatpair","Pairwise comparison:", choices = 
                                                                       if(is.null(datapair)==FALSE){
                  datapair %>% dplyr::select(trt.pair) %>% unique() 
                  }else{
                    choi<-NULL
                    }
                )), 
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
              ))),
          
          shiny::tabPanel(
            "Network" ,
            shinyjs::hidden(
              shiny::div(
                id = "netupdate",
                shiny::fluidRow(shiny::column(
                  width = 6,
                  plotly::plotlyOutput("netply")
                )),
                shiny::fluidRow(shiny::column(
                  width = 12 , shiny::verbatimTextOutput("click")
                )))
            )
            
            
          )
          
        )
        
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
        
      )
      
    )
    
    server = function(input, output, session) {
      
      
      ###############
      #   TAB 1     #
      ###############
      responsesDir <- file.path("tools")
      
      output$downproto = shiny::downloadHandler(
        filename = 'myprotocol.pdf',
        
        content = function(file) {
          
          # browser()
          # tmp <- tempdir()
          
          tmp <- system.file(package="metawRite")
          tempReport <- file.path(tmp,"inputpr2.Rnw")
          file.copy(file.path(tmp, "inputpr.Rnw"), tempReport, overwrite = TRUE)
          dir <- system.file(package="metawRite")
          
          
          writeLines(input$titleproto, con = file.path(dir, "_titleproto.Rnw"))
          
          writeLines(input$introproto, con = file.path(dir, "_introproto.Rnw"))
          writeLines(input$methodproto, con = file.path(dir, "_methodproto.Rnw"))
          out = knitr::knit2pdf(input = tempReport,
                                output = file.path(tmp, "inputpr.tex"),
                                clean = TRUE)
          file.rename(out, file) # move pdf to file for downloading
        }
        
      )
      
      
      ###See how to download Rnw file
      
      output$downprotornw = shiny::downloadHandler(
        filename = 'myprotocol.Rnw',

        content = function(file) {

          tmp <- system.file(package="metawRite")
          tempReport <- file.path(tmp,"inputpr2.Rnw")
          file.copy(file.path(tmp, "inputpr.Rnw"), tempReport, overwrite = TRUE)
          dir <- system.file(package="metawRite")

          writeLines(input$titleproto, con = file.path(dir, "_titleproto.Rnw"))

          writeLines(input$introproto, con = file.path(dir, "_introproto.Rnw"))
          writeLines(input$methodproto, con = file.path(dir, "_methodproto.Rnw"))
          # out = knitr::knit2pdf(input = tempReport,
          #                       output = file.path(tmp, "inputpr.tex"),
          #                       clean = TRUE)
          out = tempReport
          file.rename(out, file) # move Rnw to file for downloading
        }

      )
       
      #Make reactive the new information in the report
      
      
      titleproto <- shiny::reactive({
        list("titleproto",input$titleproto)
      })
      
      introproto <- shiny::reactive({
        list("introproto", input$introproto)
      })
      
      methodproto <- shiny::reactive({
        list("methodproto", input$methodproto)
      })
      
      Time<- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
      protoaux <- list("titleproto",  "introproto", "methodproto")
      
      saveData <- function(data,cc,proto=TRUE) {
        if(length(data[[2]] > 0)){
          if(!file.exists("tools")) system(sprintf("mkdir %s", "tools"))
          if(proto){
            fileName <- paste("tools/","pr",Time(),data[[1]],".txt", sep="")
          }else{
            fileName <- paste("tools/",Time(),data[[1]],".txt", sep="")
          }
          fileConn <- file(fileName)
          writeLines(data[[2]], fileConn)
          close(fileConn)
        }else{
          fileName <- paste("tools/",Time(),cc,".txt", sep="")
          fileConn <- file(fileName)
          writeLines(input$noquote(cc), fileConn)
          close(fileConn)
          
        }
        
      }
      
      # action to take when submit button is pressed
      
      shiny::observeEvent(input$submitproto, {
        
        # Save the new information in  the report  in a txt with name = date and time
        saveData(titleproto(), protoaux[[1]], proto=TRUE)
        saveData(introproto(), protoaux[[2]], proto = TRUE)
        saveData(methodproto(), protoaux[[3]], proto=TRUE)
        
        shinyjs::reset("formproto")
        shinyjs::hide("formproto")
        shinyjs::show("thankyou_msgproto")
      })
      
      
      # action to take when write new report in each textAreaInput
      
      shiny::observeEvent(input$updateproto,{
        x <- input$updateproto
        titleprotoPath <- file.path(paste("tools/",x, protoaux[[1]],".txt", sep=""))
        introprotoPath<- file.path(paste("tools/",x, protoaux[[2]],".txt", sep=""))
        methodprotoPath<- file.path(paste("tools/",x, protoaux[[3]],".txt", sep=""))
        
        titleprotoUpdate <- readLines(titleprotoPath)
        introprotoUpdate <- readLines(introprotoPath)
        methodprotoUpdate <- readLines(methodprotoPath)
        
        shiny::updateTextAreaInput(session, "titleproto", value = titleprotoUpdate)
        shiny::updateTextAreaInput(session, "introproto", value = introprotoUpdate)
        shiny::updateTextAreaInput(session, "methodproto", value = methodprotoUpdate)
      })
      
      
    
      #it is the 
      if(initialprotocol==FALSE){
        
        shinyjs::show("updateproto")
        output$updateproto <- shiny::renderUI({
          # reactiveFileReader(1000,)
          filenames <- sort(dir("tools"),TRUE)
          #filter only with pr
          auxpr <-substr(filenames, 1,2)=="pr"
          reportnamesproto <- unique(substr(filenames, 1,17)[auxpr])
          shiny::selectInput("updateproto", "Update report", reportnamesproto)
        })
        
        shiny::observeEvent(input$submit_anotherproto, {
          
          shinyjs::show("updateproto")
          shinyjs::show("formproto")
          shinyjs::hide("thankyou_msgproto")
          
          output$updateproto <- shiny::renderUI({
            # reactiveFileReader(1000,)
            filenames <- sort(dir("tools"),TRUE)
            #filter only with pr
            auxpr <-substr(filenames, 1,2)=="pr"
            reportnamesproto <- unique(substr(filenames, 1,17)[auxpr])
            shiny::selectInput("updateproto", "Update report", reportnamesproto)
          })
          
        })
        
      }else{
        
        # action to take when a submit another button is pressed
        shiny::observeEvent(input$submit_anotherproto, {
          
          shinyjs::show("updateproto")
          shinyjs::show("formproto")
          shinyjs::hide("thankyou_msgproto")
          
          output$updateproto <- shiny::renderUI({
            # reactiveFileReader(1000,)
            filenames <- sort(dir("tools"),TRUE)
            #filter only with pr
            auxpr <-substr(filenames, 1,2)=="pr"
            reportnamesproto <- unique(substr(filenames, 1,17)[auxpr])
            shiny::selectInput("updateproto", "Update report", reportnamesproto)
          })
          
        })
      }
      ###############
      #   TAB 2     #
      ###############
      
      word2<- shiny::eventReactive(input$wordButton, {input$serchtext})
      
      output$wordtext <-shiny::renderTable({
        d1<-input$date1
        d2<-input$date2
        res <- RISmed::EUtilsSummary(word2(), type="esearch", db="pubmed", datetype='pdat', mindate=d1, maxdate=d2, retmax=500)
        fetch <- RISmed::EUtilsGet(res, type = "efetch", db ="pubmed")
        numb <- RISmed::QueryCount(res)
        articles <-data.frame('Abstract'= RISmed::AbstractText(fetch))
        abstracts <-as.character(articles$Abstract)
        abstracts <-paste(abstracts, sep ="", collapse = "####Abstract####") 
        title <- RISmed::ArticleTitle(RISmed::EUtilsGet(res))
        year <- RISmed::YearPubmed(RISmed::EUtilsGet(res))
        author <- RISmed::Author(RISmed::EUtilsGet(res))
        #lastname <- sapply(author, function(x)paste(x$LastName))
        lastforestname <- sapply(author, function(x)paste(x$LastName, x$ForeName))
        result <- paste(1:numb, ")", "Title:", title,",", lastforestname, ",", year,  sep = "\n")
        result
        #wordcloud::wordcloud(abstracts, min.freq=2, max.words=70, colors=RColorBrewer::brewer.pal(7,"Dark2"))
        
      })
      
      
      ###############
      #   TAB 2 2    #
      ###############
      
      word2Ag<- shiny::eventReactive(input$wordButtonAg, {input$serchtextag})
      
      output$wordtextAg <-shiny::renderTable({
        d1<-input$date1ag
        d2<-input$date2ag
        # dirAg <- "https://api.nal.usda.gov/pubag/rest/search/?query=title:"
        # aux <- paste(dirAg, word2Ag(),"&api_key=DEMO_KEY", sep = "")
        query <- "https://api.nal.usda.gov/pubag/rest/search/?query=QQQ&api_key=DEMO_KEY"
        title <- paste("title:",  gsub("\\s+","%20",word2Ag()), sep="")
        
       
        current_query <- gsub("QQQ", title, query)
          #paste(dirAg,gsub("\\s", "", word2Ag()),"&api_key=DEMO_KEY", sep = "")
        
        # res <- RISmed::EUtilsSummary(word2Ag(), type="esearch", db="pubmed", datetype='pdat', mindate=d1, maxdate=d2, retmax=500)
        # fetch <- RISmed::EUtilsGet(res, type = "efetch", db ="pubmed")
        # numb <- RISmed::QueryCount(res)
        # articles <-data.frame('Abstract'= RISmed::AbstractText(fetch))
        # abstracts <-as.character(articles$Abstract)
        # abstracts <-paste(abstracts, sep ="", collapse = "####Abstract####") 
        # title <- RISmed::ArticleTitle(RISmed::EUtilsGet(res))
        # year <- RISmed::YearPubmed(RISmed::EUtilsGet(res))
        # author <- RISmed::Author(RISmed::EUtilsGet(res))
        # lastname <- sapply(author, function(x)paste(x$LastName))
        # result <- paste(1:numb, ")", "Title:", title,",", lastname, ",", year,  sep = "\n")
        # result
        
        #dat <-rvest::read_html(aux)
        results <- jsonlite::fromJSON(current_query)[[4]]$title
        #outtxt<-dat %>% rvest::html_text()%>%stringr::str_split("title")
        #print(outtxt[[1]][-c(1:2)])
        print(results)
      })
      
      ###############
      #   TAB 3     #
      ###############
      output$download = shiny::downloadHandler(
        #output$download <- observeEvent(input$download, {shiny::downloadHandler(
        filename = 'myreport.pdf',
        
        content = function(file) {
          
          # browser()
          # tmp <- tempdir()
          
          tmp <- system.file(package = "metawRite")
          tempReport <- file.path(tmp,"input2.Rnw")
          file.copy(file.path(tmp, "input.Rnw"), tempReport, overwrite = TRUE)
          dir <- system.file(package = "metawRite")
          
          
          writeLines(input$title, con = file.path(dir, "_title.Rnw"))
          writeLines(input$abstract, con = file.path(dir, "_abstract.Rnw"))
          writeLines(input$introduction, con = file.path(dir, "_introduction.Rnw"))
          writeLines(input$method, con = file.path(dir, "_methods.Rnw"))
          writeLines(input$result, con = file.path(dir, "_results.Rnw"))
          writeLines(input$discussion, con = file.path(dir, "_discussion.Rnw"))
          writeLines(input$funding, con = file.path(dir, "_funding.Rnw"))
          out = knitr::knit2pdf(input  = tempReport,
                                output = file.path(tmp, "input.tex"),
                                clean  = TRUE)
          file.rename(out, file) # move pdf to file for downloading
        }
        
      )
      
      # })
      
      
      
      
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
      
    
      
      #Timereport <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
      ccaux <- list("title", "abstract", "introduction", "method", "result", "discussion", "funding")
      # 
      #     saveData <- function(data,cc) {
      #       if(length(data[[2]] > 0)){
      #         if(!file.exists("tools")) system(sprintf("mkdir %s", "tools"))
      # 
      #         fileName <- paste("tools/",Timereport(),data[[1]],".txt", sep="")
      #         fileConn <- file(fileName)
      #         writeLines(data[[2]], fileConn)
      #         close(fileConn)
      #       }else{
      #         fileName <- paste("tools/",Timereport(),cc,".txt", sep="")
      #         fileConn <- file(fileName)
      #         writeLines(input$noquote(cc), fileConn)
      #         close(fileConn)
      # 
      #       }
      # 
      #     }
      
      # action to take when submit button is pressed
      
      shiny::observeEvent(input$submit, {
        
        # Save the new information in  the report  in a txt with name = date and time
        saveData(title(), ccaux[[1]], proto =FALSE)
        saveData(abstract(), ccaux[[2]], proto =FALSE)
        saveData(introduction(), ccaux[[3]], proto =FALSE)
        saveData(method(), ccaux[[4]], proto =FALSE)
        saveData(result(), ccaux[[5]], proto =FALSE)
        saveData(discussion(), ccaux[[6]], proto =FALSE)
        saveData(funding(), ccaux[[7]], proto =FALSE)
        
        
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
      
      
      
      if(initialreport==FALSE){
        shinyjs::show("reportupdate")
        
        output$update <- shiny::renderUI({
          # reactiveFileReader(1000,)
          filenames <- sort(dir("tools"),TRUE)
          auxpr2 <-substr(filenames, 1,2)!="pr"
          reportnames <- unique(substr(filenames, 1,15)[auxpr2])
          shiny::selectInput("update", "Update report", reportnames)
        })
        
        shiny::observeEvent(input$submit_another, {
          
          shinyjs::show("reportupdate")
          shinyjs::show("form")
          shinyjs::hide("thankyou_msg")
          
          output$update <- shiny::renderUI({
            # reactiveFileReader(1000,)
            filenames <- sort(dir("tools"),TRUE)
            auxpr2 <-substr(filenames, 1,2)!="pr"
            reportnames <- unique(substr(filenames, 1,15)[auxpr2])
            shiny::selectInput("update", "Update report", reportnames)
          })
          
        })
        
      }else{
        # action to take when a submit another button is pressed
        
        shiny::observeEvent(input$submit_another, {
          
          shinyjs::show("reportupdate")
          shinyjs::show("form")
          shinyjs::hide("thankyou_msg")
          
          output$update <- shiny::renderUI({
            # reactiveFileReader(1000,)
            filenames <- sort(dir("tools"),TRUE)
            auxpr2 <-substr(filenames, 1,2)!="pr"
            reportnames <- unique(substr(filenames, 1,15)[auxpr2])
            shiny::selectInput("update", "Update report", reportnames)
          })
          
        })}
      
      
      ###############
      #   TAB 4     #
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
      
      if(pair){
        shinyjs::show("pairupdate")
      }
      
      if( is.null(data)==FALSE ){
        
      up <- NULL
      sel <- datapair %>% dplyr::filter(up %in% "1") %>% dplyr::select(trt.pair) %>% unique()
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
      }
      ###############
      #   TAB 5     #
      ###############
      
      if(net){
        shinyjs::show("netupdate")
      }
      # output$netupdate <- shiny::renderUI({
      #   
      # 
      # })
      
      if(is.null(data) == FALSE){
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
    }
    
    
    
    shiny::shinyApp(ui,server)
    
  }