#' Restruscture data and pairwise meta-analysis model results using metafor package
#'
#' @usage pairwise_metafor(dataini,nupdate = 1, treat1, treat2, seTE, nobs = NULL, yi, vi, sei, ... )
#' @param dataini Data frame in contrast based format, if you have the data in arm-based you can use
#' pairwise function from netmeta package. The column names 
#' @param nupdate number of updates
#' @param nobs a vector with the number of observations for each update
#' @param yi vector of length k with the observed effect sizes or outcomes. See ‘Details’ in rma function fom metafor package.
#' @param vi vector of length k with the corresponding sampling variances. See ‘Details’ in rma function fom metafor package.
#' @param sei vector of length k with the corresponding standard errors (only relevant when not using vi). See ‘Details’in rma function fom metafor package.
#' @param ... optional argument to functions, you can include any parameter to run rma function from metafor pkg
#' @return returns to .Rdata one with tde data set in contrast-based format and the second is a list with the pairwise meta analysis for each update and each pair of treatments
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{load("./data/MTCdata.rda")
#' MTCpairs <- netmeta::pairwise(list(treat1, treat2, treat3),
#'                 list(event1, event2, event3),
#'               list(n1, n2, n3),
#'                 data = MTCdata,
#'                 sm = "RR")
#' MTCpairsrg <- netmeta::pairwise(list(t1, t2, t3, t4),
#'                 TE = list(y1, y2, y3,y4),
#'               seTE = list(se1, se2, se3,se4),
#'                 data = dat_rungano,
#'                 sm = "MD")
#' modstr <- pairwise_metafor(MTCpairs, nupdate = 2, nobs = c(109, 5), method  = "REML", measure = "RR")
#' 
#' modstr2 <- pairwise_metafor(MTCpairsrg, nupdate = 1, nobs = 29, method  = "REML", measure = "GEN")
#'  }

pairwise_metafor <- function(dataini, nupdate = 1, treat1, treat2, seTE, nobs = NULL, yi, vi, sei, ... ) {
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
 if(nupdate > 1){
   MTCpairs2 <-  dataini %>% dplyr::mutate(up = rep(c(1:nupdate), nobs) ) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(id = 1:nrow(dataini), vi = seTE^2) %>%
    plyr::ddply( plyr::.(id), function(x){
      aux <-   stringr::str_sort(x[1,] %>% dplyr::select(treat1, treat2))
      dplyr::mutate(x, trt.pair =  stringr::str_c(aux[1] ,aux[2], sep ="-"))
      }
    )

   update <- list()
   for(i in 1:length(unique(MTCpairs2$up)) ) {
     update[[i]] <- MTCpairs2  %>% dplyr::filter(up<=i) %>% 
       plyr::dlply(plyr::.(trt.pair), function(x) {
         list(x, metafor::rma(yi = TE, vi = vi, data = x))
       }
     )
   }
   
 }else{
  
   MTCpairs2 <-  dataini %>% dplyr::mutate(up = rep(1, nobs) ) %>% 
     dplyr::mutate_if(is.factor, as.character) %>%
     dplyr::mutate(id = 1:nrow(dataini), vi = seTE^2) %>%
     plyr::ddply( plyr::.(id), function(x){
       aux <-   stringr::str_sort( x[1,] %>% dplyr::select(treat1, treat2) )
       dplyr::mutate(x, trt.pair =  stringr::str_c(aux[1] ,aux[2], sep ="-") )
       
      }
     )   
   update <- MTCpairs2  %>% 
     plyr::dlply(plyr::.(trt.pair), function(x){
       list(x, metafor::rma(yi = TE, vi = vi, data = x))
     }
     )
}

list( MTCpairs2, update)
}
