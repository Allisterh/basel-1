#' maturity adjustment factor b
#'
#' The maturity adjustment factor b according to Sub-Section 2 Article 153 1. iii)
#'
#' @param PD enter the PD as a decimal number
#'
#' @return  A Number
#' @export
#'
#' @examples
#'
#' b(0.02) - for PD of 2%
#'
#'
b <- function(PD){
  stopifnot("PD input must be numeric" = is.numeric(PD))
  (0.11852-0.05478 * log(PD))^2
}

