#' Helper function to restructure the data in arm-based format if is not
#'
#' @usage dtarmbased(data, study,arms )
#' @param data Data frame with the treatment information in one column.
#' @param study	Name of the column that contain the study information.
#' @param arms Name of the column that contain arm information.
#' @return returns a data frame with arm-based format (one row for each study, and one treatment in each column)
#' @importFrom magrittr %>%
#' @export
#' @examples
#'  \dontrun{
#' data(dat.begg1989)
#' dtarmbased(dat.begg1989, study,arms )
#' }
dtarmbased <- function(data, study, arms){
  Var <- NULL
  Val <- NULL
  VarTr <- NULL
  n <- NULL
  
   data %>% tidyr::gather( Var, Val, -study, -arms) %>% 
    dplyr::group_by(study, Var) %>% 
    dplyr::mutate(n = dplyr::row_number() ) %>%
    tidyr::unite(VarTr, Var, n, sep = "") %>%
    tidyr::spread(VarTr, Val, fill = 0)

  }    
    