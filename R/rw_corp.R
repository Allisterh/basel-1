#' Risk weight for non defaulted exposure for to corporates, institutions and central governments and central banks
#'
#' Calculated the risk weight according to Sub-Section 2 Article 153 1. iii) for non defaulted exposures (0 < PD < 1).
#'
#'
#' @param PD enter the PD as a decimal number
#' @param LGD enter the LGD as a decimal number
#' @param AVC enter the AVC factor to 1.25 for all exposures to large financial sector entities or to unregulated financial
#' sector entities.
#' default value is 1.
#' @param S default value is 1.06 according to the basel formula. You can change the value to 1, for example, to simulate the effects
#' from basel IV
#' @param M maturity value (M) according to article 162 no 1.m Institutions that have not received permission to use own LGDs and
#' own conversion factors for exposures to corporates, institutions or central governments and central banks shall assign to exposures
#' arising from repurchase transactions or securities or commodities lending or borrowing transactions a maturity value (M) of 0,5 years
#' and to all other exposures M of 2,5 years.
#'
#' !!! In the actual Version, calculation is only implemented with the factor 2.5.
#'
#' default value is 2.5.
#'
#'
#' @return  Risk weigt as Number
#' @export
#'
#' @examples
#'
#' #corporate exposure secured by real estate.
#' PD <- 0.002
#' LGD <- 0.35
#' rw_corp(PD,LGD)
#'
#' #corporate exposure secured by real estate. Simulate Basel IV effects without factor 1.06#'
#' PD <- 0.002
#' LGD <- 0.35
#' rw_corp(PD,LGD, S=1)
#'
#' #corporate exposure secured by real estate to to large financial sector entities with AVC = 1.25.
#' PD <- 0.002
#' LGD <- 0.35
#' rw_corp(PD,LGD, AVC = 1.25)
#'
#'
rw_corp <- function(PD,LGD, AVC=1, S=1.06, M=2.5){
  stopifnot("PD input must be numeric" = is.numeric(PD))
  stopifnot("LGD input must be numeric" = is.numeric(LGD))
  b <- b(PD)
  R <- corr_R(PD, AVC = AVC)
  (LGD * stats::pnorm(1 / sqrt(1 - R) * stats::qnorm(PD) + sqrt(R / (1 - R)) * stats::qnorm(0.999)) - LGD * PD) * (1+(M-2.5) * b) / (1-1.5 *b)* 12.5 * S
}
