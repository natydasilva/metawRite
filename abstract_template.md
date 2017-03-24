---
title: "metawRite: Review, write and update meta-analysis results"
author: |
   | Natalia da Silva^1^ and Heike Hofmann^1^, Annette O'Connor^1^
   |
   | 1. Iowa State University  
   |
institute: 
   - $^1$Statistic Department
   - $^2$Veterinary College
output: html_document

---

**Keywords**: Living systematic review, meta-analysis, shiny, reproducible research

**Webpages**: https://github.com/natydasilva/metaupdate
Systematic reviews are used to understand how treatments are effective and to design disease control polycies, this approach is used by public health agencies such as the World Health Organization. Systematic reviews in the literature often include a meta-analysis that summarizes the findings of multiple studies. It is critical that such reviews are updated quikly as new scientific information becomes available, so the best evidence is used for treatment advice. However, the current peer-reviewed journal based approach to publishing systemic reviews means that reviews can rapidly become out of date and updating is often delayed by the publication model. Living systematic reviews have been proposed as a new approach to dealing with this problem. The main concept of a living review is to enable rapid updating of systematic reviews as new research becomes available, while also ensuring a transparent process and reproducible review. Our approach to a living systematic review will be implemented in an R package named **metawRite**. The goal is to combine writing and analysis of the review,  allowining versioning and updating in a seamless *R* package .
 **metawRite** package  will allow an easy and effective way to display a living systematic review available in a web-based display. 
Three main tasks are needed to have an effective living systematic review: the ability to produce dynamic reports, availability online with an interface that enables end users to understand the data and the ability to efficiently update the review (and any meta-analyis) with new research.
 **metawRite** package  will cover these three task integrated in a friendly web based environment for the final user. This package is not a new meta-analysis package instead will be flexible enough to read different output models from the most used meta-analysis packages in *R*, organize the information and display the results in an user driven interactive dash board. The main function of this package will display a modern web-based application for update a living systematic review.
This package combines the power of *R*,  **shiny** and  **knitr** to get a dynamic reports and up to date meta-analysis results remaining user friendly. The package has the potential to be used by a large number of groups that conduct and update systematic review such as What Works clearing house (https://ies.ed.gov/ncee/WWC/)  which reviews education interventions, Campbell Collaboration https://www.campbellcollaboration.org that includes reviews on topics such as social and criminal justice issues and many other social science topics, the Collaboration for Environment Evidence (http://www.environmentalevidence.org) and food production and security (http://www.syreaf.org) among others. 


# References
