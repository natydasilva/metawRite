#' Restruscture data and pairwise meta-analysis model results using metafor package
#'
#' @usage pairwise_metafor(armbased = TRUE, treat, event, n, mean, sd, TE, seTE, time,
#' data = NULL, studlab = NULL, incr = 0.5, allincr = FALSE, addincr = FALSE, 
#' allstudies = FALSE, nupdate = 1, nobs = NULL, yi, vi, sei, ...)
#' @param armbased A logical indicating if the data are in arm-based format (one row for each study, and one treatment in each column )
#' @param treat	A list or vector with treatment information for individual treatment arms (see Details in netmeta).
#' @param event	A list or vector with information on number of events for individual treatment arms (see Details in netmeta).
#' @param n	A list or vector with information on number of observations for individual treatment arms (see Details in netmeta).
#' @param mean	A list or vector with estimated means for individual treatment arms (see Details in netmeta).
#' @param sd	A list or vector with information on the standard deviation for individual treatment arms (see Details in netmenta).
#' @param TE	A list or vector with estimated treatment effects for individual treatment arms (see Details in netmta).
#' @param seTE	A list or vector with standard errors of estimated treatment effect for individual treatment arms (see Details in netmta).
#' @param time	A list or vector with information on person time at risk for individual treatment arms (see Details in netmeta).
#' @param data Data frame in contrast-based or arm-based format 
#' @param studlab	A vector with study labels (optional).
#' @param incr	A numerical value which is added to each cell frequency for studies with a zero cell count.
#' @param allincr	A logical indicating if incr is added to each cell frequency of all studies if at least one study has a zero cell count. If FALSE (default), incr is added only to each cell frequency of studies with a zero cell count.
#' @param addincr	A logical indicating if incr is added to each cell frequency of all studies irrespective of zero cell counts.
#' @param allstudies	A logical indicating if studies with zero or all events in two treatment arms are to be included in the meta-analysis (applies only if sm is equal to "RR" or "OR").
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
#' \dontrun{
#' MTCpairs <- pairwise(list(treat1, treat2, treat3),
#'                 list(event1, event2, event3),
#'               list(n1, n2, n3),
#'                 data = MTCdata,
#'                 sm = "RR")
#' MTCpairsrg <- pairwise(list(t1, t2, t3, t4),
#'                 TE = list(y1, y2, y3,y4),
#'               seTE = list(se1, se2, se3, se4),
#'                 data = dat_rungano,
#'                 sm = "MD")
#' modstr <- pairwise_metafor(MTCpairs, nupdate = 2, treat1 = treat1, 
#' treat2 = treat2, nobs = c(109, 5), method  = "REML", measure = "RR")
#' modstr <- pairwise_metafor(armbased = TRUE, treat=list(treat1, treat2, treat3),
#'                event= list(event1, event2, event3),
#'               n=list(n1, n2, n3),
#'                 data = MTCdata, nupdate = 2,  nobs = c(109, 5), measure = "RR")
#' 
#' modstr <- pairwise_metafor(armbased = TRUE, treat=trt, TE=yi,seTE=sei, studlab=study,
#' data = dat.begg1989, nupdate = 1,  nobs = 20, measure = "GEN")
#'                            
#' 
#' modstr2 <- pairwise_metafor(MTCpairsrg, nupdate = 1, treat1 = treat1, 
#' treat2 = treat2, nobs = 29, method  = "REML", measure = "GEN")
#'  }

pairwise_metafor <- function(armbased = TRUE,
                             treat, event, n, mean, sd, TE, seTE, time,
                             data = NULL, studlab = NULL,
                             incr = 0.5, allincr = FALSE, addincr = FALSE, allstudies = FALSE,
                              nupdate = 1, nobs = NULL, yi, vi, sei, ... ) {

  # seTE <- NULL
   id <-  NULL
   treat1 <- NULL
   treat2 <- NULL
   up  <- NULL
   trt.pair <- NULL
  # rma <- NULL
  # TE <- NULL
  # vi <- NULL
  # data <- NULL

#construct a new data set with the variable trt.pair unique pair of treatments and save the data

#to use pairwise from netmet
  listcontrol <- NULL
  logicontrol <- NULL
  gs <- NULL
  metabin <- NULL
  metacont <- NULL
  metagen <- NULL
  metainc <- NULL
  
  #controls
  logicontrol <- function (x, name = NULL) 
  {
    if (is.null(name)) 
      name <- deparse(substitute(x))
    if (is.numeric(x)) 
      x <- as.logical(x)
    if (length(x) != 1 || !is.logical(x) || is.na(x)) 
      stop("Argument '", name, "' must be a logical.", call. = FALSE)
    invisible(NULL)
  }
  
  numericontrol <- function (x, min, max, zero = FALSE, single = FALSE, name = NULL) 
  {
    if (is.null(name)) 
      name <- deparse(substitute(x))
    x <- x[!is.na(x)]
    if (length(x) == 0) 
      return(invisible(NULL))
    if (!(is.numeric(x))) 
      stop("Non-numeric value for argument '", name, "'.", 
           call. = FALSE)
    if (single & length(x) != 1) 
      stop("Argument '", name, "' must be a numeric of length 1.", 
           call. = FALSE)
    if (!missing(min) & missing(max)) {
      if (zero & min == 0 & any(x <= min, na.rm = TRUE)) 
        stop("Argument '", name, "' must be positive.", call. = FALSE)
      else if (any(x < min, na.rm = TRUE)) 
        stop("Argument '", name, "' must be larger equal ", 
             min, ".", call. = FALSE)
    }
    if (missing(min) & !missing(max)) {
      if (zero & max == 0 & any(x >= max, na.rm = TRUE)) 
        stop("Argument '", name, "' must be negative.", call. = FALSE)
      else if (any(x > max, na.rm = TRUE)) 
        stop("Argument '", name, "' must be smaller equal ", 
             min, ".", call. = FALSE)
    }
    if ((!missing(min) & !missing(max)) && (any(x < min, na.rm = TRUE) | 
                                            any(x > max, na.rm = TRUE))) 
      stop("Argument '", name, "' must be between ", min, " and ", 
           max, ".", call. = FALSE)
    invisible(NULL)
  }
  
  effset <- function (x, val, text, list = FALSE, name = NULL) 
  {
    if (is.null(name)) 
      name <- deparse(substitute(x))
    nval <- length(val)
    idx <- charmatch(tolower(x), tolower(val), nomatch = NA)
    if (any(is.na(idx)) || any(idx == 0)) {
      if (list) 
        first <- "List element '"
      else first <- "Argument '"
      if (missing(text)) {
        if (nval == 1) 
          vlist <- paste("\"", val, "\"", sep = "")
        else if (nval == 2) 
          vlist <- paste("\"", val, "\"", collapse = " or ", 
                         sep = "")
        else vlist <- paste(paste("\"", val[-nval], "\"", 
                                  collapse = ", ", sep = ""), ", or ", "\"", val[nval], 
                            "\"", sep = "")
        stop(first, name, "' should be ", vlist, ".", call. = FALSE)
      }
      else stop(first, name, "' ", text, ".", call. = FALSE)
    }
    val[idx]
  }
  
  listcontrol <- function (x) 
  {
    if (!is.list(x)) 
      stop("Argument '", deparse(substitute(x)), "' must be a list.", 
           call. = FALSE)
  }
  
  
  if (is.null(data))
    data <- sys.frame(sys.parent())
  mf <- match.call()
  studlab <- eval(mf[[match("studlab", names(mf))]], data,
                  enclos = sys.frame(sys.parent()))
  treat <- eval(mf[[match("treat", names(mf))]], data, enclos = sys.frame(sys.parent()))
  event <- eval(mf[[match("event", names(mf))]], data, enclos = sys.frame(sys.parent()))
  n <- eval(mf[[match("n", names(mf))]], data, enclos = sys.frame(sys.parent()))
  mean <- eval(mf[[match("mean", names(mf))]], data, enclos = sys.frame(sys.parent()))
  sd <- eval(mf[[match("sd", names(mf))]], data, enclos = sys.frame(sys.parent()))
  TE <- eval(mf[[match("TE", names(mf))]], data, enclos = sys.frame(sys.parent()))
  seTE <- eval(mf[[match("seTE", names(mf))]], data, enclos = sys.frame(sys.parent()))
  time <- eval(mf[[match("time", names(mf))]], data, enclos = sys.frame(sys.parent()))
  args <- list(...)
  nam.args <- names(args)
  
  
  if (is.null(treat)) 
    stop("Argument 'treat' mandatory.")
  if (is.list(treat)) 
    listcontrol(treat)
  if (!is.null(event)) 
    if (is.list(event)) 
      listcontrol(event)
  else numericontrol(event)
  if (!is.null(n)) 
    if (is.list(n)) 
      listcontrol(n)
  else numericontrol(n)
  if (!is.null(mean)) 
    if (is.list(mean)) 
      listcontrol(mean)
  else numericontrol(mean)
  if (!is.null(sd)) 
    if (is.list(sd)) 
      listcontrol(sd)
  else numericontrol(sd)
  if (!is.null(TE)) 
    if (is.list(TE)) 
      listcontrol(TE)
  else numericontrol(TE)
  if (!is.null(seTE)) 
    if (is.list(seTE)) 
      listcontrol(seTE)
  else numericontrol(seTE)
  if (!is.null(time)) 
    if (is.list(time)) 
      listcontrol(time)
  else numericontrol(time)
  numericontrol(incr, min = 0, single = TRUE)
  logicontrol(allincr)
  logicontrol(addincr)
  logicontrol(allstudies)
  if (!is.null(event) & !is.null(n) & is.null(mean) & is.null(sd) & 
      is.null(TE) & is.null(seTE) & is.null(time)) 
    type <- "binary"
  else if (is.null(event) & !is.null(n) & !is.null(mean) & 
           !is.null(sd) & is.null(TE) & is.null(seTE) & is.null(time)) 
    type <- "continuous"
  else if (!is.null(event) & is.null(n) & is.null(mean) & is.null(sd) & 
           is.null(TE) & is.null(seTE) & !is.null(time)) 
    type <- "count"
  else if (is.null(event) & is.null(n) & is.null(mean) & is.null(sd) & 
           !is.null(TE) & !is.null(seTE) & is.null(time)) 
    type <- "generic"
  else stop("Type of outcome unclear. Please provide the necessary information:\n  - event, n (binary outcome)\n  - n, mean, sd (continuous outcome)\n  - TE, seTE (generic outcome)\n  - event, time (incidence rates).")
  treat.list <- list()
  event.list <- list()
  n.list <- list()
  mean.list <- list()
  sd.list <- list()
  TE.list <- list()
  seTE.list <- list()
  time.list <- list()
  if (type == "binary") {
    listformat <- is.list(event) & is.list(n)
    if (!listformat) {
      if (is.null(studlab)) 
        stop("Argument 'studlab' mandatory if argument 'event' is a vector.")
      ttab <- table(as.character(studlab), as.character(treat))
      n.arms <- apply(ttab, 1, sum)
      tdat <- data.frame(studlab, treat, event, n, stringsAsFactors = FALSE)
      tdat <- tdat[order(tdat$studlab, tdat$treat), ]
      studlab <- names(n.arms)
      tres <- data.frame(studlab = studlab, stringsAsFactors = FALSE)
      for (i in 1:max(n.arms)) {
        tdat.i <- tdat[!duplicated(tdat$studlab), ]
        tres.i <- merge(tres, tdat.i, by = "studlab", 
                        all.x = TRUE)
        treat.list[[i]] <- tres.i$treat
        event.list[[i]] <- tres.i$event
        n.list[[i]] <- tres.i$n
        tdat <- tdat[duplicated(tdat$studlab), ]
      }
      treat <- treat.list
      event <- event.list
      n <- n.list
    }
  }

 #Transform the data from arm-based to contrast based 
  if(armbased){
 MTCpairs <- netmeta::pairwise(treat = treat, event = event, n = n, mean = mean, sd = sd, TE = TE, seTE = seTE,
                      time = time, data = data, studlab = studlab ,
                      incr = incr , allincr = allincr, addincr = addincr, allstudies = allstudies,
                      ...)  
 dataini <- MTCpairs 
  }
 
  
   MTCpairs2 <-  dataini %>% dplyr::mutate(up = rep(c(1:nupdate), nobs) ) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(id = 1:nrow(dataini), vi = seTE^2) %>% 
     # plyr::ddply( plyr::.(id), function(x){
     #   aux <-   stringr::str_sort(x[1,] %>% dplyr::select(treat1, treat2))
     #   dplyr::mutate(x, trt.pair =  stringr::str_c(aux[1] ,aux[2], sep ="-"))
     # }
     # )
      dplyr::group_by(id) %>%   tidyr::nest() %>% dplyr::mutate(trt.pair = purrr::map(data,function(x){
      aux <-   stringr::str_sort(x[1,] %>% dplyr::select(treat1, treat2))
        stringr::str_c(aux[1] ,aux[2], sep ="-")}))%>% tidyr::unnest()
    

   update <- MTCpairs2  %>% dplyr::group_by(trt.pair ) %>%
     tidyr::nest() %>% 
     dplyr::mutate(model = purrr::map(data, function(d) {
       purrr::map(.x = sort(unique(d$up)),  .f = function(x) 
         metafor::rma(yi = TE, vi = vi, data = dplyr::filter(d, up <= x) ) )
       }))
              
            

list( MTCpairs2, update)
}
