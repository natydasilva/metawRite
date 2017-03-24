---
title: "Title of your submission"
author: |
   | Author A^1^ and Author B^1,2^
   |
   | 1. Affiliation of author A and author B
   | 2. Second affiliation of author B
institute: 
   - $^1$Juans Casa
   - $^2$Tus Place
output: html_document
bibliography: biblioExample.bib
nocite: | 
  @ref2, @ref3
---

**Keywords**: First, Second, ..., up to 5 keywords

**Webpages**: https://CRAN.R-project.org/package=mypkg, https://rpubs.com/username/project

This abstract template can be rendered as *HTML* using the **rmarkdown** package, e.g.

    library(rmarkdown)
    render("abstract_template.md")
    
or using the shortcuts provided in your IDE for *R*.

Some suggestions: if you mention a programming language like *R*, italicize the language name.  If you mention an *R* function `foo`, typeset the function name as code.   If you mention an *R* package **fooPkg**, typeset the package name in bold.  Textual citations, e.g., @ref1 jumped over the fence, and parenthetical citations, e.g., the fence was jumped [@ref1], may appear within the abstract. If you want to include references that are not cited in the text, use the `nocite` metadata field. Note the 
references do not need to be stored as a `.bib` file, see [Bibliographies and Citations](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html) in the R Markdown documentation for alternatives. Also note that using a `.bib` does not require *LaTeX* to be installed. Markup such as code blocks, displayed equations, and lists should be used sparingly (if at all), to keep the abstract short. Abstracts should not exceed one side of A4 paper when printed and should not include images.

# References
