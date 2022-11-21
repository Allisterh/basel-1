
#' Coefficient of correlation R
#'
#' The coefficient of correlation R according to Sub-Section 2 Article 153 1. iii for exposures to corporates,
#' institutions, central governments, central banks and Article 154 4. for SME exposure.
#'
#'
#'
#' @param PD Enter the PD as a decimal number.
#' @param AVC Set the AVC factor to 1.25 for all exposures to large financial sector entities or to unregulated financial
#' sector entities.
#' Default value is 1.
#' @param SME SME exposure = 1: For SME exposures to companies where the total annual sales for the consolidated group of which the firm is a part
#' is less than EUR 50 million, the formula according to Article 153 4. is used. You have to enter the annual sales in parameter S.
#'
#' Default value is 0 for non SME exposure.
#' @param S S is expressed as total annual sales in millions of Euro. It is only used when parameter SME = 1
#'
#' @return  A Number
#' @export
#'
#' @examples
#' #standard
#' PD <- 0.002
#' corr_R(PD)
#'
#' #exposure to large financial sector entities
#' PD <- 0.002
#' corr_R(PD, AVC=1.25)
#'
#' #exposures to SME
#' PD<- 0.002
#' SME = 1
#' S = 30 #30 million euro annual sales
#' corr_R(PD, SME=1, S=30)
#'
#'
corr_R <- function (PD, AVC = 1, SME=0, S=0){
  stopifnot("PD input must be numeric" = is.numeric(PD))
  AVC * ((0.12 * (1 - exp(-50*PD))+0.24*(1-(1-exp(-50*PD))/(1-exp(-50)))) - SME * (0.04 * (1- (pmin(pmax(5,S),50)-5)/45)))
}

