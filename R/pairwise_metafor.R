#' Restruscture data and pairwise meta-analysis model results using metafor package
#'
#' @usage pairwise_metafor(dataini, ...)
#' @param dataini Data frame in an arm-based format (e.g. input format for WinBUGS)
#' @param ... optional argument to functions, you can include any parameter to run rma function from metafor pkg
#' @return returns to .Rdata one with tde data set in contrast-based format and the second is a list with the pairwise meta analysis for each update and each pair of treatments
#' @importFrom magrittr %>%
#' @export
# @examples
# load("./data/MTCdata.rda")
# MTCpairs <- netmeta::pairwise(list(treat1, treat2, treat3),
#                 list(event1, event2, event3),
#               list(n1, n2, n3),
#                 data = MTCdata,
#                 sm = "RR")
# #Include study updates id
# MTCpairs <- data.frame(up = c(rep(1, 109), rep(2, 5)), MTCpairs)
# pairwise_metafor(MTCpairs,  method  = "REML",measure="RR")

pairwise_metafor <- function(dataini, ... ) {
  seTE <- NULL
  id <-  NULL
  treat1 <- NULL
  treat2 <- NULL
  up  <- NULL
  trt.pair <- NULL
  rma <- NULL
  TE <- NULL
  vi <- NULL
#construct a new data set with the variable trt.pair unique pair of treatments and save the data
  MTCpairs2 <- dataini %>% dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(id = 1:nrow(dataini), vi = seTE^2) %>%
    plyr::ddply( plyr::.(id), function(x){
      aux <-   stringr::str_sort(x[1,] %>% dplyr::select(treat1,treat2))

      dplyr::mutate(x, trt.pair =  stringr::str_c(aux[1],aux[2],sep ="-"))

    }
    )

  save(MTCpairs2, file ="./data/MTCpairs2.rda")

update <- MTCpairs2 %>% dplyr::filter(up%in%"1")%>%
  plyr::dlply(plyr::.(trt.pair), function(x)

    list(x,rma( yi = TE, vi = vi, data = x))

  )

  update <- list()
for(i in 1:length(unique(MTCpairs2$up))){
update<- MTCpairs2  %>% dplyr::filter(up<=i) %>% plyr::dlply(plyr::.(trt.pair), function(x)

  list(x, rma(yi = TE, vi = vi,data=x))

  )
pair_result <- list(update, update)
}

save(pair_result, file = "./data/pair_result.rda")

}
