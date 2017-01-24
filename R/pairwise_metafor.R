#' Restruscture data and pairwise meta-analysis model results using metafor package
#'
#'\code{pairwise_metafor} implements .....
#' @usage pairwise_metafor(dataini, ...)
#' @param dataini Data frame with .....define... pairwise data info using pair function}
#' @param ... optional argument to functions, you can include any parameter to run rma function from metafor pkg
#' @return return a list with information needed for metapairwise
#' @importFrom magrittr %>%
#' @export
#' @examples
#' load("./data/MTCdata.Rdata")
#' MTCpairs <- pairwise(list(treat1, treat2, treat3),
#'                 list(event1, event2, event3),
#'                 list(n1, n2, n3),
#'                 data = MTCdata,
#'                 sm = "RR")
#'MTCpairs <- data.frame(up = c(rep(1, 109), rep(2, 5)), MTCpairs)
#' pairwise_metafor(MTCpairs,  method = "REML")


pairwise_metafor <- function(dataini, ... ) {

#construct a new data set with the variable trt.pair unique pair of treatments and save the data
  MTCpairs2 <- dataini %>% mutate_if(is.factor, as.character) %>%
    mutate(id = 1:nrow(dataini), vi = seTE^2) %>%
    ddply( .(id), function(x){
      aux <-   str_sort(x[1,] %>%select(treat1,treat2))

      mutate(x, trt.pair = str_c(aux[1],aux[2],sep ="-"))

    }
    )

  save(MTCpairs2, file ="./data/MTCpairs2.Rdata")



update<- MTCpairs2 %>% filter(up%in%"1")%>%
  dlply(.(trt.pair), function(x)

    list(x,rma( yi = TE, vi = vi, data = x))

  )

  update <- list()
for(i in 1:length(unique(up))){
update<- MTCpairs2  %>% filter(up<=i) %>% dlply(.(trt.pair), function(x)

  list(x, rma(yi = TE, vi = vi,data=x))

  )
pair_result <- list(update, update)
}

#pair_result <- list(update, update)

save(pair_result, file = "./data/pair_result.Rdata")

}
