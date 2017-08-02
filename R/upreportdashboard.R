#' Meta-analysis reportshiny app, using shinydashboard (improved version)
#'
#' @usage upreportdashoard(initialprotocol = TRUE, initialreport = TRUE, pair=FALSE,
#' net = FALSE, data = NULL, outputformat = "pdf",clearproto = FALSE)
#' @param initialprotocol logical value to indicate if it is the initial protocol, by default is TRUE.
#' @param initialreport logical value to indicate if it is the initial review, by default is TRUE.
#' @param pair logical value to indicate if pairwaise analysis should be run, by default it is FALSE. To run pair is needed to specify the data.
#' @param net logical value to indicate if the analysisi will include a network meta-analysis, by default it is FALSE.
#' @param data list with two components, a data frame with treatment information for the pairwise meta-analysis (treat1 and treat2), id to identify each observation
#' and trt.pair with the string name for the pairwise comparison in alphabetic order, generated using pairwise_metafor in data folder. The second
#' element is a list with the pairwise meta-analysis models generated using pairwise_metafor in data folder
#' @param outputformat format to download protocol and report
#' @param clearproto only keep the latest protocol file
#' @importFrom magrittr %>%
#' @export
#' @examples
#'\dontrun{
#' 
#' upreportdashoard(initialprotocol = TRUE, initialreport = TRUE,pair =FALSE,
#' net = FALSE, data = NULL,outputformat="pdf", clearproto =FALSE)  
#'  upreportdashoard(initialprotocol = TRUE, initialreport = TRUE, pair =TRUE,
#'   net = FALSE, data = modstr,outputformat = "pdf")
#' }
upreportdashoard <-
  function(initialprotocol = TRUE, initialreport =TRUE,  pair=FALSE,net = FALSE, data = NULL, outputformat="pdf", clearproto =FALSE) {
    
    if(is.null(data)){
      datapair <-NULL
      pair_result <- NULL
      trt.pair <- NULL
      treat1 <- NULL
      id <- NULL
    }else{
      datapair <- data[[1]]
      pair_result <- data[[2]]
      trt.pair <- data[[1]]$trt.pair
      treat1 <- data[[1]]$treat1
      treat2 <- data[[1]]$treat2
      id <- data[[1]]$id
      
    }
    
    if(initialprotocol == FALSE & clearproto==TRUE){
        filenames <- sort(dir("tools"),TRUE)
        #filter only with pr
        auxpr <-substr(filenames, 1,2) == "pr"
        reportnamesproto <- which.max(lubridate::ymd_hms(sub("-", "", unique(substr(filenames, 3,17)[auxpr] ))))

        filenamesaux <-     filenames[reportnamesproto:(reportnamesproto+2)]
        unlink(paste("tools/",setdiff(filenames,filenamesaux), sep=
                       ""))
    }
    
    lsr <- list(title = '',
                abstract = '',
                introduction = '',
                method = '',
                result = '',
                discussion = '',
                funding = '')
    
    protocol <- list(titleprotoident = "",
                     titleprotoup = "", 
                     registration = "",
                     authorcontact = "",
                     authorcontri = "",
                     amendments = "",
                     supportsorce = "",
                     supportsponsor = "",
                     supportrole = "",
                     introprotorat = "",
                     introprotoobj = "",
                     methodprotoeli = "",
                     methodprotoinfo = "",
                     methodprotosear = "",
                     methodprotodataman = "",
                     methodprotosele = "",
                     methodprotodatacol = "",
                     methodprotodatait = "",
                     methodprotout = "",
                     methodprotorisk = "",
                     methodprotodatasy = "",
                     methodprotometa = "",
                     methodprotoconfi = ""
                     )
    

header <- shinydashboard::dashboardHeader(title = "metawRite")

sidebar <-  shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(id="welcome",
   # shinydashboard::menuItem("Welcome", tabName = "welcome", id="welcome"),
    shinydashboard::menuItem("Motivation", tabName ="welcome"),
    #shinydashboard::menuItem("LSR", tabName = "lrs", id = "lrs"),
   # shinyBS::bsTooltip("LSR", "Living Systematic Review", placement = "bottom", trigger = "hover"),
    shinydashboard::menuItem("Protocol", tabName = "protocol"),
   shinydashboard::menuItem("Search", tabName = "search"),
    shinydashboard::menuSubItem("PubMed", tabName = "pubmed"),
    shinydashboard::menuSubItem("PubAg", tabName = "pubagr"),
   # shinydashboard::menuSubItem("EuroPubMed", tabName = "pubeuro"),
    shinydashboard::menuItem("LSR-report", tabName = "report"),
    shinydashboard::menuItem("Pairwise", tabName = "pairwise"),
    shinydashboard::menuItem("Network", tabName = "network")
    #shinyBS::bsTooltip(id = 'Welcome', title = "This is an input", options = list(container = 'body'))

    #, 
             #sidebarMenuOutput("menu")
    
    
    
  )  )


 tmp <- system.file(package = "metawRite")
 tempReport <- file.path(tmp,"motivation.Rmd")
 file.copy(file.path(tmp, "motivation2.Rmd"), tempReport, overwrite = TRUE)
 dir <- system.file(package = "metawRite")

#Package motivation
tab1 <-  
  shinydashboard::tabItem(tabName = "welcome",
          shiny::includeMarkdown(file.path(dir, "motivation.Rmd"))
)

#Initial step in a LSR, write a protocol
tab2 <- shinydashboard::tabItem(tabName = "protocol",
                shinyjs::hidden(
                  shiny::div(style="display:inline-block",
                    id = "updateproto",
                    shiny::uiOutput("updateproto"))
                ),
                shiny::div(
                  id = "formproto",
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'titleprotoident',
                      'Title: Identification',
                      rows = 1,
                      width = "900px",
                      value = protocol$titleprotoident, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'titleprotoup',
                      'Title: Update',
                      rows = 1,
                      width = "900px",
                      value = protocol$titleprotoup, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'registration',
                      'Registration',
                      rows = 1,
                      width = "900px",
                      value = protocol$registration, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'authorcontact',
                      'Author: Contact',
                      rows = 3,
                      width = "900px",
                      value = protocol$contact, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'authorcontri',
                      'Author: Contributions',
                      rows = 4,
                      width = "900px",
                      value = protocol$authorcontri, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'amendments',
                      'Amendments',
                      rows = 2,
                      width = "900px",
                      value = protocol$amendments, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'supportsorce',
                      'Support: Sources',
                      rows = 1,
                      width = "900px",
                      value = protocol$supportsorce, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'supportsponsor',
                      'Support: Sponsor',
                      rows = 1,
                      width = "900px",
                      value = protocol$supportsponsor, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'supportrole',
                      'Support: Role of sponsor or funder',
                      rows = 1,
                      width = "900px",
                      value = protocol$supportrole, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'introprotorat',
                      'Introduction: Rationale',
                      rows = 4,
                      width = "900px",
                      value = protocol$introprotorat, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'introprotoobj',
                      'Introduction: Objective',
                      rows = 4,
                      width = "900px",
                      value = protocol$introprotoobj, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'methodprotoeli',
                      'Methods: Eligibility Criteria',
                      rows = 5,
                      width = "900px",
                      value = protocol$methodprotoeli, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'methodprotoinfo',
                      'Methods: Information Sources',
                      rows = 5,
                      width = "900px",
                      value = protocol$methodprotoinfo, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'methodprotoinfo',
                      'Methods: Information Sources',
                      rows = 5,
                      width = "900px",
                      value = protocol$methodprotoinfo, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'methodprotosear',
                      'Methods: Search Strategy',
                      rows = 5,
                      width = "900px",
                      value = protocol$methodprotosear, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'methodprotodataman',
                      'Methods: Data Management',
                      rows = 5,
                      width = "900px",
                      value = protocol$methodprotodataman, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'methodprotosele',
                      'Methods: Selection processss',
                      rows = 5,
                      width = "900px",
                      value = protocol$methodprotosele, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'methodprotodatacol',
                      'Methods: Data Collection Process',
                      rows = 5,
                      width = "900px",
                      value = protocol$methodprotodatacol, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'methodprotodatait',
                      'Methods: Data Items',
                      rows = 5,
                      width = "900px",
                      value = protocol$methodprotodatait, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'methodprotout',
                      'Methods: Outcomes and Prioritizations',
                      rows = 5,
                      width = "900px",
                      value = protocol$methodprotout, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'methodprotorisk',
                      'Methods: Risk of Bias in individual studies',
                      rows = 5,
                      width = "900px",
                      value = protocol$methodprotorisk, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'methodprotodatasy',
                      'Methods: Data synthesis',
                      rows = 5,
                      width = "900px",
                      value = protocol$methodprotodatasy, resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'methodprotometa',
                      'Methods: Meta Bias',
                      rows = 5,
                      width = "900px",
                      value = protocol$methodprotometa , resize ="vertical")
                  )),
                  shiny::fluidRow(shiny::column(
                    8,
                    shiny::textAreaInput(
                      'methodprotoconfi',
                      'Methods: Confidence in Cumulative Evidence',
                      rows = 5,
                      width = "900px",
                      value = protocol$methodprotoconfi, resize ="vertical")
                  )),
                
                  
                  shiny::actionButton("submitproto", "Submit protocol", class = "btn-primary"),
                  shiny::downloadButton(outputId='downproto', label="Download")
                  #shiny::uiOutput("buttreset")
                           
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
                    shiny::actionLink("submit_anotherproto", "Submit another protocol")
                  )
                )
) 

#Search module, should I include all the search only in one tab
tab3 <-  shinydashboard::tabItem(tabName = "pubmed",
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::helpText("Type a word below to search in NBC database, you can search authors, topics, any acronym, etc"),
      shiny::textInput("serchtext", label = shiny::h3("Keywords"), value = "pinkeye in cows"),
      shiny::helpText("String indicating the NCBI database used in query, can be any valid Entrez database, i.e. pubmed, pmc, protein, nlm, etc. "),
      shiny::textInput("database", label = shiny::h3("NBC database"), value = "pubmed"),
      shiny::helpText("Specify the start and end dates of your search, use the format YYYY/MM/DD"),
      shiny::textInput("date1", label = shiny::h3("From"),value="2012/01/01"),
      shiny::textInput("date2", label = shiny::h3("To"),  value = "2016/01/01"),
      shiny::helpText("Now select serch and you can see the paper title, authors and publication year"),
      shiny::actionButton("wordButton","Search")),
    
    shiny::mainPanel(
      shiny::HTML("<div style='height: 50px;'>"),
      shiny::HTML("</div>"),
      shiny::tableOutput("wordtext")
    )))

# PubAg search, fich dates
tab4 <- shinydashboard::tabItem(tabName = "pubagr",
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::helpText("Type a word below to search in PubAg, you can search keyworkds"),
      shiny::textInput("serchtextag", label = shiny::h3("Keywords"), value = "pinkeye"),
      shiny::helpText("Specify the publication year of your search, use the format YYYY"),
      shiny::textInput("date1ag", label = shiny::h3("From"),value="2012"),
      shiny::textInput("date2ag", label = shiny::h3("To"),  value = "2016"),
      shiny::helpText("Now select serch and you can see the paper title, authors and publication year"),
      shiny::actionButton("wordButtonAg","Search")),
    
    shiny::mainPanel(
      shiny::HTML("<div style='height: 50px;'>"),
      shiny::HTML("</div>"),
      shiny::tableOutput("wordtextAg")
    )))

tab4aux <- shinydashboard::tabItem(tabName = "pubeuro",
                                shiny::sidebarLayout(
                                  shiny::sidebarPanel(
                                    shiny::helpText("Type a word below to search in Euro PubMed, you can search ...."),
                                    shiny::textInput("serchtextpeuro", label = shiny::h3("Keywords"), value = "pinkeye"),
                                    shiny::helpText("Specify the publication year of your search, use the format YYYY"),
                                    shiny::textInput("date1euro", label = shiny::h3("From"),value="2012"),
                                    shiny::helpText("Now select serch and you can see the paper title, authors and publication year"),
                                    shiny::actionButton("wordButtonEuro","Search")),
                                  
                                  shiny::mainPanel(
                                    shiny::HTML("<div style='height: 50px;'>"),
                                    shiny::HTML("</div>"),
                                    shiny::tableOutput("wordtextEuro")
                                  )))

tab5 <- shinydashboard::tabItem(tabName = "report",
  shinyjs::hidden(
    shiny::div(
      id = "reportupdate",
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
)

tab6 <- shinydashboard::tabItem(tabName = "pairwise",
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
      
    
      shiny::fluidRow(
        shiny::column(width =  6, shiny::plotOutput("forest2" ) ),
        shiny::column(width =  6, shiny::plotOutput("funel2" ) )
        
      ),
      shiny::fluidRow(shiny::column(
        width =  10, shiny::verbatimTextOutput("summary2")
      ))
    )))


tab7 <-  shinydashboard::tabItem(tabName = "network",
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

ui <- shinydashboard::dashboardPage(skin = "purple",
                    header,
                    sidebar,
                    shinydashboard::dashboardBody( 
                      shinyjs::useShinyjs(),
                      shinydashboard::tabItems(
                      tab1,
                      tab2,
                      tab3,
                      tab4,
                      tab5,
                      tab6,
                      tab7
                    )))


server <- function(input, output, session) {

    ###############
    #   TAB 1     #
    ###############

    responsesDir <- file.path("tools")
    if(outputformat=="word"){
      outputformataux <- "docx"
    filenameout <- paste("myprotocol",".", outputformataux,sep="")
    }else{
      filenameout <- paste("myprotocol",".", outputformat,sep="")  
    }
    output$downproto = shiny::downloadHandler(
      filename = filenameout,
      
      content = function(file){
  
        tmp <- system.file(package = "metawRite")
        
        tempReport <- file.path(tmp,"inputpr2.Rmd")
        file.copy(file.path(tmp, "inputpr.Rmd"), tempReport, overwrite = TRUE)
        dir <- system.file(package="metawRite")
        
        #xx <- stringr::str_split(input$titleproto)
       
        writeLines(input$titleprotoident, con = file.path(dir, "_titleprotoident.Rmd"), sep = "\n")
        writeLines(input$titleprotoup, con = file.path(dir, "_titleprotoup.Rmd"), sep = "\n")
        writeLines(input$registration, con = file.path(dir, "_registration.Rmd"), sep = "\n")
        writeLines(input$authorcontact, con = file.path(dir, "_authorcontact.Rmd"),sep = "\n")
        writeLines(input$authorcontri, con = file.path(dir, "_authorcontri.Rmd"),sep = "\n")
        writeLines(input$amendments, con = file.path(dir, "_amendments.Rmd"),sep = "\n")
        writeLines(input$supportsorce, con = file.path(dir, "_supportsorce.Rmd"),sep = "\n") 
        writeLines(input$supportsponsor, con = file.path(dir, "_supportsponsor.Rmd"),sep = "\n") 
        writeLines(input$supportrole, con = file.path(dir, "_supportrole.Rmd"),sep = "\n")
        writeLines(input$introprotorat, con = file.path(dir, "_introprotorat.Rmd"),sep = "\n")
        writeLines(input$introprotoobj, con = file.path(dir, "_introprotoobj.Rmd"),sep = "\n")
        writeLines(input$methodprotoeli, con = file.path(dir, "_methodprotoeli.Rmd"),sep = "\n")
        writeLines(input$methodprotoinfo, con = file.path(dir, "_methodprotoinfo.Rmd"),sep = "\n")
        writeLines(input$methodprotosear, con = file.path(dir, "_methodprotosear.Rmd"),sep = "\n")
        writeLines(input$methodprotodataman, con = file.path(dir, "_methodprotodataman.Rmd"),sep = "\n")
        writeLines(input$methodprotosele, con = file.path(dir, "_methodprotosele.Rmd"),sep = "\n")
        writeLines(input$methodprotodatacol, con = file.path(dir, "_methodprotodatacol.Rmd"),sep = "\n")
        writeLines(input$methodprotodatait, con = file.path(dir, "_methodprotodatait.Rmd"),sep = "\n")
        writeLines(input$methodprotout, con = file.path(dir, "_methodprotout.Rmd"),sep = "\n")
        writeLines(input$methodprotorisk, con = file.path(dir, "_methodprotorisk.Rmd"),sep = "\n")
        writeLines(input$methodprotodatasy, con = file.path(dir, "_methodprotodatasy.Rmd"),sep = "\n")
        writeLines(input$methodprotometa, con = file.path(dir, "_methodprotometa.Rmd"),sep = "\n")
        writeLines(input$methodprotoconfi, con = file.path(dir, "_methodprotoconfi.Rmd"),sep = "\n")
        
        
        outform <- paste(outputformat, "_","document",sep="")
         out = rmarkdown::render(input = tempReport,output_format= outform,
                               clean = TRUE)
        file.rename(out, file) # move pdf to file for downloading
      }
   
    )
    
    #Make reactive the new information in the report
    
    titleprotoident <- shiny::reactive({
      list("titleprotoident", input$titleprotoident)
    })
    
    titleprotoup <- shiny::reactive({
      list("titleprotoup", input$titleprotoup)
    })
    
    registration <- shiny::reactive({
      list("registration", input$registration)
    })
    
    authorcontact <- shiny::reactive({
      list("authorcontact", input$authorcontact)
    })
    
    authorcontri <- shiny::reactive({
      list("authorcontri", input$authorcontri)
    })
    
    amendments <- shiny::reactive({
      list("amendments", input$amendments)
    })
    
    supportsorce <- shiny::reactive({
      list("supportsorce", input$supportsorce)
    })
    
    supportsponsor <- shiny::reactive({
      list("supportsponsor", input$supportsponsor)
    })
    
    supportrole <- shiny::reactive({
      list("supportrole", input$supportrole)
    })
    
    introprotorat <- shiny::reactive({
      list("introprotorat", input$introprotorat)
    })
    
  
    introprotoobj <- shiny::reactive({
      list("introprotoobj", input$introprotoobj)
    })
    methodprotoeli <- shiny::reactive({
      list("methodprotoeli", input$methodprotoeli)
    })
    methodprotoinfo <- shiny::reactive({
      list("methodprotoinfo", input$methodprotoinfo)
    })
    methodprotosear <- shiny::reactive({
      list("methodprotosear", input$methodprotosear)
    })
    methodprotodataman <- shiny::reactive({
      list("methodprotodataman", input$methodprotodataman)
    })
    
    methodprotosele <- shiny::reactive({
      list("methodprotosele", input$methodprotosele)
    })
    methodprotodatacol <- shiny::reactive({
      list("methodprotodatacol", input$methodprotodatacol)
    })
    
    methodprotodatait <- shiny::reactive({
      list("methodprotodatait", input$methodprotodatait)
    })
    
    methodprotout <- shiny::reactive({
      list("methodprotout", input$methodprotout)
    })
    
    methodprotorisk <- shiny::reactive({
      list("methodprotorisk", input$methodprotorisk)
    })
    
    methodprotodatasy <- shiny::reactive({
      list("methodprotodatasy", input$methodprotodatasy)
    })
    
    methodprotometa <- shiny::reactive({
      list("methodprotometa", input$methodprotometa)
    })
    
    methodprotoconfi <- shiny::reactive({
      list("methodprotoconfi", input$methodprotoconfi)
    })
  
  
    Time <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
    protoaux <- list("titleprotoident", "titleprotoup", "registration", "authorcontact",
                     "authorcontri", "amendments", "supportsorce", "supportsponsor", 
                     "supportrole", "introprotorat", "introprotoobj", "methodprotoeli",
                     "methodprotoinfo" ,"methodprotosear","methodprotodataman",
                     "methodprotosele" ,"methodprotodatacol","methodprotodatait",
                     "methodprotout","methodprotorisk","methodprotodatasy",
                     "methodprotometa","methodprotoconfi" )
    
    saveData <- function(data,cc,proto=TRUE) {
      if(length(data[[2]] > 0)){
        if(!file.exists("tools")) system(sprintf("mkdir %s", "tools"))
        if(proto){
          fileName <- paste("tools/","pr",Time(),data[[1]],".txt", sep="")
        }else{
          fileName <- paste("tools/",Time(),data[[1]],".txt", sep="")
        }
        fileConn <- file(fileName)
        writeLines(data[[2]], fileConn, sep="\n")
        close(fileConn)
      }else{
        fileName <- paste("tools/",Time(),cc,".txt", sep="")
        fileConn <- file(fileName)
        writeLines(input$noquote(cc), fileConn, sep = "\n")
        close(fileConn)
        
      }
      
    }
    
    # action to take when submit button is pressed
    
    shiny::observeEvent(input$submitproto, {
      
      # Save the new information in  the report  in a txt with name = date and time
      saveData(titleprotoident(), protoaux[[1]], proto = TRUE)
      saveData(titleprotoup(), protoaux[[2]], proto = TRUE)
      saveData(registration(), protoaux[[3]], proto = TRUE)
      saveData(authorcontact(), protoaux[[4]], proto = TRUE)
      saveData(authorcontri(), protoaux[[5]], proto = TRUE)
      saveData(amendments(), protoaux[[6]], proto = TRUE)
      saveData(supportsorce(), protoaux[[7]], proto = TRUE)
      saveData(supportsponsor(), protoaux[[8]], proto = TRUE)
      saveData(supportrole(), protoaux[[9]], proto = TRUE)
      saveData(introprotorat(), protoaux[[10]], proto = TRUE)
      saveData(introprotoobj(), protoaux[[11]], proto = TRUE)
      saveData(methodprotoeli(), protoaux[[12]], proto=TRUE)
      saveData(methodprotoinfo(), protoaux[[13]], proto=TRUE)
      saveData(methodprotosear(), protoaux[[14]], proto=TRUE)
      saveData(methodprotodataman(), protoaux[[15]], proto=TRUE)
      saveData(methodprotosele(), protoaux[[16]], proto=TRUE)
      saveData(methodprotodatacol(), protoaux[[17]], proto=TRUE)
      saveData(methodprotodatait(), protoaux[[18]], proto=TRUE)
      saveData(methodprotout(), protoaux[[19]], proto=TRUE)
      saveData(methodprotorisk(), protoaux[[20]], proto=TRUE)
      saveData(methodprotodatasy(), protoaux[[21]], proto=TRUE)
      saveData(methodprotometa(), protoaux[[22]], proto=TRUE)
      saveData(methodprotoconfi(), protoaux[[23]], proto=TRUE)
      
    
      shinyjs::reset("formproto")
      shinyjs::hide("formproto")
      shinyjs::show("thankyou_msgproto")
    })
    
    
    # action to take when write new report in each textAreaInput
    
    shiny::observeEvent(input$updateproto,{
      x <- input$updateproto
      titleprotoidentPath <- file.path(paste("tools/",x, protoaux[[1]],".txt", sep = ""))
      titleprotoupPath <- file.path(paste("tools/",x, protoaux[[2]],".txt", sep = ""))
      registrationPath <- file.path(paste("tools/",x, protoaux[[3]],".txt", sep = ""))
      authorcontactPath <- file.path(paste("tools/",x, protoaux[[4]],".txt", sep = ""))
      authorcontriPath <- file.path(paste("tools/",x, protoaux[[5]],".txt", sep = ""))
      amendmentsPath <- file.path(paste("tools/",x, protoaux[[6]],".txt", sep = ""))
      supportsorcePath <- file.path(paste("tools/",x, protoaux[[7]],".txt", sep = ""))
      supportsponsorPath <- file.path(paste("tools/",x, protoaux[[8]],".txt", sep = ""))
      supportrolePath <- file.path(paste("tools/",x, protoaux[[9]],".txt", sep = ""))
      introprotoratPath <- file.path(paste("tools/",x, protoaux[[10]],".txt", sep = ""))
      introprotoobjPath <- file.path(paste("tools/",x, protoaux[[11]],".txt", sep=""))
      methodprotoeliPath <- file.path(paste("tools/",x, protoaux[[12]],".txt", sep = ""))
      methodprotoinfoPath <- file.path(paste("tools/",x, protoaux[[13]],".txt", sep = ""))
      methodprotosearPath <- file.path(paste("tools/",x, protoaux[[14]],".txt", sep = ""))
      methodprotodatamanPath <- file.path(paste("tools/",x, protoaux[[15]],".txt", sep = ""))
      methodprotoselePath <- file.path(paste("tools/",x, protoaux[[16]],".txt", sep = ""))
      methodprotodatacolPath <- file.path(paste("tools/",x, protoaux[[17]],".txt", sep = ""))
      methodprotodataitPath <- file.path(paste("tools/",x, protoaux[[18]],".txt", sep = ""))
      methodprotoutPath <- file.path(paste("tools/",x, protoaux[[19]],".txt", sep = ""))
      methodprotoriskPath <- file.path(paste("tools/",x, protoaux[[20]],".txt", sep = ""))
      methodprotodatasyPath <- file.path(paste("tools/",x, protoaux[[21]],".txt", sep = ""))
      methodprotometaPath <- file.path(paste("tools/",x, protoaux[[22]],".txt", sep = ""))
      methodprotoconfiPath <- file.path(paste("tools/",x, protoaux[[23]],".txt", sep = ""))

      
      titleprotoidentUpdate <- paste(readLines(titleprotoidentPath), collapse = '\n')
      titleprotoupUpdate <- paste(readLines(titleprotoupPath), collapse = '\n')
      registrationUpdate <- paste(readLines(registrationPath), collapse = '\n')
      authorcontactUpdate <- paste(readLines(authorcontactPath), collapse = '\n')
      authorcontriUpdate <- paste(readLines(authorcontriPath), collapse = '\n')
      amendmentsUpdate <- paste(readLines(amendmentsPath), collapse = '\n')
      supportsorceUpdate <- paste(readLines(supportsorcePath), collapse = '\n')
      supportsponsorUpdate <- paste(readLines(supportsponsorPath), collapse = '\n')
      supportroleUpdate <- paste(readLines(supportrolePath), collapse = '\n')
      introprotoratUpdate <- paste(readLines(introprotoratPath), collapse = '\n')
      introprotoobjUpdate <- paste(readLines(introprotoobjPath), collapse = '\n')
      methodprotoeliUpdate <- paste(readLines(methodprotoeliPath), collapse ='\n')
      methodprotoinfoUpdate <- paste(readLines(methodprotoinfoPath), collapse ='\n')
      methodprotosearUpdate <- paste(readLines(methodprotosearPath), collapse ='\n')
      methodprotodatamanUpdate <- paste(readLines(methodprotodatamanPath), collapse ='\n')
      methodprotodatamanUpdate <- paste(readLines(methodprotodatamanPath), collapse ='\n')
      methodprotoseleUpdate <- paste(readLines(methodprotoselePath), collapse ='\n')
      methodprotodatacolUpdate <- paste(readLines(methodprotodatacolPath), collapse ='\n')
      methodprotodataitUpdate <- paste(readLines(methodprotodataitPath), collapse ='\n')
      methodprotoutUpdate <- paste(readLines(methodprotoutPath), collapse ='\n')
      methodprotoriskUpdate <- paste(readLines(methodprotoriskPath), collapse ='\n')
      methodprotodatasyUpdate <- paste(readLines(methodprotodatasyPath), collapse ='\n')
      methodprotometaUpdate <- paste(readLines(methodprotometaPath), collapse ='\n')
      methodprotoconfiUpdate <- paste(readLines(methodprotoconfiPath), collapse ='\n')

      
      shiny::updateTextAreaInput(session, "titleprotoident", value = titleprotoidentUpdate)
      shiny::updateTextAreaInput(session, "titleprotoup", value = titleprotoupUpdate)
      shiny::updateTextAreaInput(session, "registration", value = registrationUpdate)
      shiny::updateTextAreaInput(session, "authorcontact", value = authorcontactUpdate)
      shiny::updateTextAreaInput(session, "authorcontri", value = authorcontriUpdate)
      shiny::updateTextAreaInput(session, "amendments", value = amendmentsUpdate)
      shiny::updateTextAreaInput(session, "supportsorce", value = supportsorceUpdate)
      shiny::updateTextAreaInput(session, "supportsponsor", value = supportsponsorUpdate)
      shiny::updateTextAreaInput(session, "supportrole", value = supportroleUpdate)
      shiny::updateTextAreaInput(session, "introprotorat", value = introprotoratUpdate)
      shiny::updateTextAreaInput(session, "introprotoobj", value = introprotoobjUpdate)
      shiny::updateTextAreaInput(session, "methodprotoeli", value = methodprotoeliUpdate)
      shiny::updateTextAreaInput(session, "methodprotoinfo", value = methodprotoinfoUpdate)
      shiny::updateTextAreaInput(session, "methodprotosear", value = methodprotosearUpdate)
      shiny::updateTextAreaInput(session, "methodprotodataman", value = methodprotodatamanUpdate)
      shiny::updateTextAreaInput(session, "methodprotosele", value = methodprotoseleUpdate)
      shiny::updateTextAreaInput(session, "methodprotodatacol", value = methodprotodatacolUpdate)
      shiny::updateTextAreaInput(session, "methodprotodatait", value = methodprotodataitUpdate)
      shiny::updateTextAreaInput(session, "methodprotou", value = methodprotoutUpdate)
      shiny::updateTextAreaInput(session, "methodprotorisk", value = methodprotoriskUpdate)
      shiny::updateTextAreaInput(session, "methodprotodatasy", value = methodprotodatasyUpdate)
      shiny::updateTextAreaInput(session, "methodprotometa", value = methodprotometaUpdate)
      shiny::updateTextAreaInput(session, "methodprotoconfi", value = methodprotoconfiUpdate)
    })
    
    
    
    # action to take when select Keep only latest version
    
    # shiny::observeEvent(input$buttreset,{
    #   
    #   filenames <- sort(dir("tools"),TRUE)
    #   #filter only with pr
    #   auxpr <-substr(filenames, 1,2) == "pr"
    #   reportnamesproto <- which.max(lubridate::ymd_hms(sub("-", "", unique(substr(filenames, 3,17)[auxpr] ))))
    #      
    #   filenamesaux <-     filenames[reportnamesproto:(reportnamesproto+2)]
    #   unlink(paste("tools/",setdiff(filenames,filenamesaux), sep=
    #                  ""))
    # })

    #it is the 
    if(initialprotocol == FALSE){
      #shinyjs::show("buttreset")
      shinyjs::show("updateproto")
      output$updateproto <- shiny::renderUI({
     
        # reactiveFileReader(1000,)
        filenames <- sort(dir("tools"),TRUE)
        #filter only with pr
        auxpr <-substr(filenames, 1,2)=="pr"
        reportnamesproto <- unique(substr(filenames, 1,17)[auxpr])
        shiny::selectInput("updateproto", "Update report", reportnamesproto)
    
      })
        
      # output$buttreset <- shiny::renderUI({
      #   actionButton("buttreset", "Keep only latest version")
      # })
      
      shiny::observeEvent(input$submit_anotherproto, {
        #shinyjs::show("buttreset")
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
        
        # output$buttreset <- shiny::renderUI({
        #   actionButton("buttreset", "Keep only latest version")
        # })
      })
      
    }else{
      
      # action to take when a submit another button is pressed
      shiny::observeEvent(input$submit_anotherproto, {
        #shinyjs::show("buttreset")
        shinyjs::show("updateproto")
        shinyjs::show("formproto")
        shinyjs::hide("thankyou_msgproto")
        
        output$updateproto <- shiny::renderUI({
          # reactiveFileReader(1000,)
         
          filenames <- sort(dir("tools"),TRUE)
          #filter only with pr
          auxpr <-substr(filenames, 1,2) == "pr"
          reportnamesproto <- unique(substr(filenames, 1,17)[auxpr])
          shiny::selectInput("updateproto", "Update protocol", reportnamesproto)
        })
        
      })
      # output$buttreset <- shiny::renderUI({
      #   actionButton("buttreset", "Keep only latest version")
      # })
    }
    
 
    ###############
    #   TAB 2     #
    ###############
    
    word2<- shiny::eventReactive(input$wordButton, shiny::isolate(input$serchtext))
    dbre <- shiny::eventReactive(input$wordButton, shiny::isolate(input$database))
    
    output$wordtext <-shiny::renderTable({
      d1 <- shiny::isolate(input$date1)
      d2 <- shiny::isolate(input$date2)
      res <- RISmed::EUtilsSummary(word2(), type="esearch", db= dbre(), datetype='pdat', mindate=d1, maxdate=d2, retmax=500)
      fetch <- RISmed::EUtilsGet(res, type = "efetch", db = dbre())
      numb <- RISmed::QueryCount(res)
      # articles <-data.frame('Abstract'= RISmed::AbstractText(fetch))
      # abstracts <-as.character(articles$Abstract)
      # abstracts <-paste(abstracts, sep ="", collapse = "####Abstract####") 
      title <- RISmed::ArticleTitle(RISmed::EUtilsGet(res))
      year <- RISmed::YearPubmed(RISmed::EUtilsGet(res))
      author <- RISmed::Author(RISmed::EUtilsGet(res))
     
      lastforestname <- sapply(author, function(x)paste(x$LastName, x$ForeName, collapse = ","))
      result <- paste(1:numb, ")", "Title:", title,",", lastforestname, ",", year,  sep = "\n")
      result
     
    })
    
    ###############
    #   TAB 2 2    #
    ###############
    
    word2Ag <- shiny::eventReactive(input$wordButtonAg, {shiny::isolate(input$serchtextag)})
    yearAg <- shiny::eventReactive(input$wordButtonAg, {shiny::isolate(input$date1ag)})
    yearAgto <- shiny::eventReactive(input$wordButtonAg, {shiny::isolate(input$date2ag)})
    allyears <- shiny::eventReactive(input$wordButtonAg, {seq(input$date1ag:input$date2ag)})
    
    output$wordtextAg <-shiny::renderTable({
      d1 <- shiny::isolate(input$date1ag)
      d2 <- shiny::isolate(input$date2ag)
  
      query <- "https://api.nal.usda.gov/pubag/rest/search/?query=QQQ&api_key=DEMO_KEY"
      title <- paste("title:",  gsub("\\s+","%20",word2Ag()), sep="")
      results <- NULL
      for(i in yearAg():yearAgto()){
      
      year <- paste("publication_year:", i, sep = "")
      search <- paste(title, year,  sep="+")
      
      current_query <- gsub("QQQ", search, query)
  
      allsearch <- jsonlite::fromJSON(current_query)
      titlesearch <- allsearch[[4]]$title
      authorsearch <- allsearch[[4]]$authors
      sourcesearch <- allsearch[[4]]$source
      
      if(is.null(titlesearch)){
        results <- results
      }else{
      results<- c(results, paste(1:length(titlesearch),")", titlesearch, authorsearch,sourcesearch))
        }
      }
      results
    })
    ###############
    #   TAB 3     #
    ###############
    output$download = shiny::downloadHandler(
    
      filename = 'myreport.pdf',
      
      content = function(file) {
        
        # browser()
        # tmp <- tempdir()
        
        tmp <- system.file(package = "metawRite")
        tempReport <- file.path(tmp,"input2.Rmd")
        file.copy(file.path(tmp, "input.Rmd"), tempReport, overwrite = TRUE)
        dir <- system.file(package = "metawRite")
        
        
        writeLines(input$title, con = file.path(dir, "_title.Rmd"))
        writeLines(input$abstract, con = file.path(dir, "_abstract.Rmd"))
        writeLines(input$introduction, con = file.path(dir, "_introduction.Rmd"))
        writeLines(input$method, con = file.path(dir, "_methods.Rmd"))
        writeLines(input$result, con = file.path(dir, "_results.Rmd"))
        writeLines(input$discussion, con = file.path(dir, "_discussion.Rmd"))
        writeLines(input$funding, con = file.path(dir, "_funding.Rmd"))
        outform <- paste(outputformat, "_","document",sep = "")
        out = rmarkdown::render(input = tempReport,output_format= outform,
                                clean = TRUE)
        file.rename(out, file) # move pdf to file for downloading
      }
      
    )
  
    
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
    
  
    ccaux <- list("title", "abstract", "introduction", "method", "result", "discussion", "funding")
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
      
      #paste(readLines(titleprotoidentPath), collapse = '\n')
      
      titleUpdate <- paste(readLines(titlePath), collapse = '\n')
      abstractUpdate <- paste(readLines(abstractPath), collapse = '\n')
      introductionUpdate <- paste(readLines(introductionPath), collapse = '\n')
      methodUpdate <- paste(readLines(methodPath), collapse = '\n')
      resultUpdate <- paste(readLines(resultPath), collapse = '\n')
      discussionUpdate <- paste(readLines(discussionPath), collapse = '\n')
      fundingUpdate <- paste(readLines(fundingPath), collapse = '\n')
      
      
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
  
    if( is.null(data)==FALSE ){
      shinyjs::show("pairupdate")
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

shiny::shinyApp(ui, server)
}