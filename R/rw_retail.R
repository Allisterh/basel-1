#' Risk weight for non defaulted retail exposures
#'
#' Calculated the risk weight according to Sub-Section 2 Article 154 1. ii) for non defaulted retail exposures (0 < PD < 1).
#' In the actual version, the correlation factor R is set to 0.15 for retail exposures secured by immovable property according to
#' to Sub-Section 2 Article 154 3..
#'
#'
#' @param PD enter the PD as a decimal number
#' @param LGD enter the LGD as a decimal number
#' @param R  R is set to 0.15 for retail exposures secured by immovable property according to
#' to Sub-Section 2 Article 154 3.
#' @param SK default is 1.06 according to the basel formula. You can change the value to 1, for example, to simulatethe effects
#' from basel IV
#'
#'
#'
#' @return  A Number
#' @export
#'
#' @examples
#'
#' PD <- 0.002
#' LGD <- 0.12
#' rw_retail(PD,LGD)
#'
#' #simulate Basel IV effects without factor 1.06#'
#' PD <- 0.002
#' LGD <- 0.12
#' rw_retail(PD,LGD, SK=1)
#'
rw_retail <- function(PD,LGD, R=0.15, SK=1.06){
  stopifnot("PD input must be numeric" = is.numeric(PD))
  stopifnot("LGD input must be numeric" = is.numeric(LGD))
  (LGD * stats::pnorm(1 / sqrt(1 - R) * stats::qnorm(PD) + sqrt(R / (1 - R)) * stats::qnorm(0.999)) - LGD * PD) * 12.5 * SK
}
