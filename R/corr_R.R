
#' coefficient of correlation R
#'
#' The coefficient of correlation R according to Sub-Section 2 Article 153 1. iii)
#' for exposures to corporates, institutions and central governments and central banks
#'
#' @param PD enter the PD as a decimal number
#' @param AVC enter the AVC factor to 1.25 for all exposures to large financial sector entities or to unregulated financial
#' sector entities.
#' default value is 1.
#'
#' @return  A Number
#' @export
#'
#' @examples
#'
#' PD <- 0.002
#' corr_R(PD)
#'
#' #exposure to large financial sector entities
#' PD <- 0.002
#' corr_R(PD, AVC=1.15)
#'
corr_R <- function (PD, AVC = 1){
  stopifnot("PD input must be numeric" = is.numeric(PD))
  AVC * (0.12 * (1 - exp(-50*PD))+0.24*(1-(1-exp(-50*PD))/(1-exp(-50))))
}

