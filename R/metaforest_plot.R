#'  Implements a forest plot using ggplot2 package.
#'
#' @usage metaforest_plot(data, Treatment, effect, cilb, ciub, size = 2)
#' @param data Data frame with the information needed for the forest plot (effect, confidence interval and reference name).
#' @param Treatment column name  with the cite information or treatment name. 
#' @param effect column name with the observed effect sizes or outcomes.
#' @param cilb column name with the lower bound ci.
#' @param ciub column name with the upper bound ci.
#' @param size points size
#' @importFrom magrittr %>%
#' @export
#' 
metaforest_plot <-  function(data, Treatment, effect, cilb, ciub, size = 2){ 
  
  Effect_ci <- paste(round(data$effect,2), " ( ",data$cilb," , ", data$ciub, " ) ", sep = "")
  
  p1 <- ggplot2::ggplot(data, ggplot2::aes(y = Treatment, x = effect, xmin = cilb, xmax = ciub , label =  Effect_ci) ) +
    ggplot2::geom_point(color = 'black', shape = 18, size = size) +  ggplot2::geom_errorbarh(height = .1) +
    ggplot2::labs(y = "Treatment", x = "Effect") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          axis.line = ggplot2::element_blank(),
          text = ggplot2::element_text(family = 'Times'),
          legend.position = 'none') + ggrepel::geom_text_repel( ggplot2::aes(x = max(data$ciub) + 15))
  
  p1
  #ggplotly(p1, tooltip = c("Treatment", "Effect_ci"))
  
}
