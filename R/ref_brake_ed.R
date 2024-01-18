#' brake_ed: Reference tables for the energy densities of all foods in the Food and Brain Study
#'
#' Energy Density for each food served during the Food and Brain Study including the standard laboratory test meal (visits 1 and 7), eating in the absence of hunger (visits 1 and 7), portion size meals (visits 2-5), and snacks served during visit 6
#'
#'
#' @docType data
#'
#' @usage data(brake_ed)
#'
#' @format A data.frame with columns:
#' \describe{
#'     \item{meal}{meal food was served at}
#'     \item{food}{food item}
#'     \item{ED}{energy denisty}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(brake_ed)
#' ED <- attr(brake_ed, "ED")
#'
"brake_ed"
