#' @keywords internal
"_PACKAGE"

#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom lazyeval lazy lazy_dots lazy_eval interp
#' @importFrom stats setNames sigma var
#' @importFrom utils read.table
#' @importFrom R.matlab readMat
#' @importFrom reshape2 melt
NULL

## quiets concerns of R CMD check about . that appears in pipelines
utils::globalVariables(c("."))
