#' Restruscture data and pairwise meta-analysis model results using metafor package
#'
#'\code{pairwise.metafor} implements .....
#' @usage pairwise.metafor(data, yi, vi, sei, weights, ai, bi, ci, di, n1i, n2i, x1i, x2i, t1i, t2i,
#'m1i, m2i, sd1i, sd2i, xi, mi, ri, ti, sdi, ni, mods,
#'measure="GEN", intercept=TRUE, data, slab, subset,
#'add=1/2, to="only0", drop00=FALSE, vtype="LS",
#'method="REML", weighted=TRUE, test="z",
#'level=95, digits=4, btt, tau2, verbose=FALSE, control, ...)
#' @param data Data frame with .....define\code{\link{ }}
#' @param narms numbers of arms in the data
#' @param nupdates numbers of updates in the meta-analysis.
#' @param yi	vector of length k with the observed effect sizes or outcomes. See ‘Details’.
#' @param vi	vector of length k with the corresponding sampling variances. See ‘Details’.
#' @param sei	vector of length k with the corresponding standard errors (only relevant when not using vi). See ‘Details’.
#' @param weights optional argument to specify a vector of length k with user-defined weights. See ‘Details’.
#' @param ai	 see below and the documentation of the escalc function for more details.
#' @param bi	see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param ci see below and the documentation of the  \code{\link{ escalc}} function for more details.
#' @param di	see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param n1i	see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param n2i	see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param x1i see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param x2i see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param t1i see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param t2i see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param m1i see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param m2i see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param sd1i see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param sd2i see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param xi see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param mi see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param ri see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param ti see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param sdi see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param ni see below and the documentation of the \code{\link{ escalc}} function for more details.
#' @param mods optional argument to include one or more moderators in the model. A single moderator can be given as a vector of length k specifying the values of the moderator. Multiple moderators are specified by giving a matrix with k rows and as many columns as there are moderator variables. Alternatively, a model formula can be used to specify the model. See ‘Details’.
#' @param measure character string indicating the type of data supplied to the function. When measure="GEN" (default), the observed effect sizes or outcomes and corresponding sampling variances (or standard errors) should be supplied to the function via the yi, vi, and sei arguments (only one of the two, vi or sei, needs to be specified). Alternatively, one can set measure to one of the effect size or outcome measures described under the documentation for the escalc function and specify the needed data via the appropriate arguments.
#' @param intercept logical indicating whether an intercept term should be added to the model (the default is TRUE).
#' @param data optional data frame containing the data supplied to the function.
#' @param slab optional vector with labels for the k studies.
#' @param subset optional vector indicating the subset of studies that should be used for the analysis. This can be a logical vector of length k or a numeric vector indicating the indices of the observations to include.
#' @param add see the documentation of the escalc function.
#' @param to see the documentation of the escalc function.
#' @param drop00 see the documentation of the escalc function.
#' @param vtype see the documentation of the escalc function.
#' @param method character string specifying whether a fixed- or a random/mixed-effects model should be fitted. A fixed-effects model (with or without moderators) is fitted when using method="FE". Random/mixed-effects models are fitted by setting method equal to one of the following: "DL", "HE", "SJ", "ML", "REML", "EB", "HS", or "GENQ". Default is "REML". See ‘Details’.
#' @param weighted logical indicating whether weighted (default) or unweighted estimation should be used to fit the model.
#' @param test character string specifying how test statistics and confidence intervals for the fixed effects should be computed. By default (test="z"), Wald-type tests and CIs are obtained, which are based on a standard normal distribution. When test="knha", the method by Knapp and Hartung (2003) is used for adjusting test statistics and confidence intervals. See ‘Details’.
#' @param level numerical value between 0 and 100 specifying the confidence interval level (the default is 95).
#' @param digits integer specifying the number of decimal places to which the printed results should be rounded (the default is 4).
#' @param btt optional vector of indices specifying which coefficients to include in the omnibus test of moderators. See ‘Details’.
#' @param tau2 optional numerical value to specify the amount of (residual) heterogeneity in a random- or mixed-effects model (instead of estimating it). Useful for sensitivity analyses (e.g., for plotting results as a function of τ²). When unspecified, the value of τ² is estimated from the data.
#' @param verbose logical indicating whether output should be generated on the progress of the model fitting (the default is FALSE). Can also be an integer. Values > 1 generate more verbose output. See ‘Note’.
#' @param control
#' @param optional list of control values for the iterative estimation algorithms. If unspecified, default values are defined inside the function. See ‘Note’.
#' @param ...additional arguments.
#' @return return a list with information needed for metapairwise
#' @importFrom magrittr %>%
#' @export
pairwise.metafor <- function(data, narms,nupdates,  yi, vi, sei, weights, ai, bi, ci, di, n1i, n2i, x1i, x2i, t1i, t2i,
                             m1i, m2i, sd1i, sd2i, xi, mi, ri, ti, sdi, ni, mods,
                             measure = "GEN", intercept = TRUE, data, slab, subset,
                             add = 1/2, to = "only0", drop00 = FALSE, vtype = "LS",
                             method = "REML", weighted = TRUE, test = "z",
                             level = 95, digits = 4, btt, tau2, verbose = FALSE, control, ... ) {


#
#
# MTCredu <- data %>%
#   select(Number.of.Event.in.arm.1,Number.of.Event.in.arm.2,Number.of.Event.in.arm.3, Total.number.in.arm.1, Total.number.in.arm.2, Total.number.in.arm.3,
#          Arm.1,Arm.2, Arm.3) %>%
#   rename(event1 =Number.of.Event.in.arm.1, event2 = Number.of.Event.in.arm.2, event3 = Number.of.Event.in.arm.3,
#          n1 = Total.number.in.arm.1, n2 = Total.number.in.arm.2, n3 = Total.number.in.arm.3, treat1 =Arm.1, treat2 = Arm.2, treat3 = Arm.3)
#
#
#
#
# MTCpairs <- pairwise(list(treat1, treat2, treat3),
#                      list(event1, event2, event3),
#                      list(n1, n2, n3),
#                      data=MTCredu,
#                      sm="RR")

MTCpairs <- data.frame(Update = c(rep("93 trials", 109), rep("98 trials", 5)), MTCpairs)


library(stringr)

library(dplyr)
#to use the stringr pkg convert factors to character and collapse str
#unique pairwise treatments
#should I use avreviation??

MTCpairs2 <- MTCpairs %>% mutate_if(is.factor, as.character) %>%
  mutate(id = 1:nrow(MTCpairs), vi = seTE^2) %>%
  ddply( .(id), function(x){
    aux <-   str_sort(x[1,] %>%select(treat1,treat2))

    mutate(x, trt.pair = str_c(aux[1],aux[2],sep ="-"))

  }
  )

save(MTCpairs2, file ="MTCpairs2.Rdata")


#apply to each pair of treatments a pairwise meta-analysis

library(metafor)
update1  <- MTCpairs2 %>% filter(Update%in%"93 trials") %>%
  dlply(.(trt.pair), function(x)
    list(x,rma(yi = TE, vi = vi, data = x, method = "REML"))
  )

update2<- MTCpairs2 %>% dlply(.(trt.pair), function(x)
  list(x,rma(yi = TE, vi = vi, data = x, method = "REML"))
)

pair_result <- list(update1, update1)

save(pair_result, file = "pair_resut.Rdata")

}
