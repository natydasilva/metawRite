


###############

server = function(input, output) {
  output$report = shiny::downloadHandler(
    filename = 'myreport.pdf',

    content = function(file) {

      #     browser()
      #     tmp <- tempdir()
      tmp <- system.file(package="metaupdate")
      tempReport <- file.path(tmp, "input2.Rnw")
      file.copy(file.path(tmp, "input.Rnw"), tempReport, overwrite = TRUE)
      dir <- system.file(package="metaupdate")


      dir <- system.file(package="metaupdate")
      writeLines(input$title, con = file.path(dir, "_title.Rnw"))
      writeLines(input$abstract, con = file.path(dir, "_abstract.Rnw"))
      out = knitr::knit2pdf(input = tempReport,
                            output = file.path(tmp, "input.tex"),
                            clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
    },

    contentType = 'inst/application/pdf'
  )
