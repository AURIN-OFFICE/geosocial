#' Concordances ABS data
#' The ABS has developed a suite of geographical correspondences,
#' primarily to assist users make comparisons and maintain
#' time series between different editions of the Australian
#' Statistical Geography Standard (ASGS). Correspondences
#' are a mathematical method of reassigning data from one
#' geographic region to another geographic region.
#'
#' This file combines the concordances
#' @format `concordances`
#' A data frame with 8149 rows and 10 columns:
#' \describe{
#'   \item{origin_unit}{Geographical unit - Origin}
#'   \item{destination_unit}{Geographical unit - Destination}
#'   \item{year_in}{Year of Geographical unit - Origin}
#'   \item{year_out}{Year of Geographical unit - Destination}
#'   \item{origin}{Geographical code - Origin}
#'   \item{destination}{Geographical code - Destination}
#'   \item{ratio}{Year}
#'   \item{origin_areasqkm}{Area in square kilometres - Origin}
#'   \item{destination_areasqkm}{Area in square kilometres - Destination}
#' }
#' @source <https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/correspondences>
"concordances"
