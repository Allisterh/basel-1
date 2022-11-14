
#' coefficient of correlation R
#'
#' The coefficient of correlation R according to Sub-Section 2 Article 153 1. iii)
#'
#' @param PD enter the PD as a decimal number
#'
#' @return  A Number
#' @export
#'
#' @examples
#'
#' PD <- 0.002
#' corr_R(PD)
#'
#'
corr_R <- function (PD){
  stopifnot("PD input must be numeric" = is.numeric(PD))
  0.12 * (1 - exp(-50*PD))+0.24*(1-(1-exp(-50*PD))/(1-exp(-50)))
}

