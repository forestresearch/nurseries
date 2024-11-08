#' Date to planting year interval.
#'
#' @author Daniel Braby
#' 
#' @description Planting years run from 1st October to 30th September the following year.
#'
#' @param date Date to get the planting year from.
#' 
#' @importFrom lubridate year
#' @importFrom lubridate interval
#' @importFrom lubridate make_date
#'
#' @return Planting year interval.
#' @export
#'
#' @author Rowan Clarke
#'
#' @examples
#' in_planting_year(as.Date("2022-05-07"))
in_planting_year <- function(date) {
  ending_year <- year(date) + ifelse(month(date) >= 10, 1, 0)
  interval(make_date(ending_year - 1, 10, 1),
           make_date(ending_year, 9, 30))
}

#' Get the planting year interval that is represented by a given year.
#'
#' @description The interval \strong{starts} in the year that is specified.
#'
#' @param year Year to convert to planting year interval.
#'
#' @return Planting year interval.
#' @export
#'
#' @examples
#' to_planting_year(2023)
to_planting_year <- function(year) {
  in_planting_year(make_date(year, 10, 1))
}
