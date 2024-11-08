#' Recursively match file paths under directory.
#'
#' @author Daniel Braby
#'
#' @description Return paths to files matching specified criteria.
#'
#' @param dir_path Directory path.
#' @param regex Matching criteria.
#'
#' @importFrom fs dir_info
#'
#' @return File paths.
#' @export
#'
files_matching <- function(dir_path, regex) {
  paths <- fs::dir_info(dir_path, recurse = TRUE)$path
  paths[grepl(regex, paths)]
}



#' Recursively match file paths under directory.
#'
#' @author Daniel Braby
#'
#' @description Return paths to files matching specified criteria.
#'
#' @param dir_path Directory path.
#' @param regex Matching criteria.
#'
#' @importFrom fs dir_info
#'
#' @return File paths.
#' @export
#'

names_diff <- function(new, old) {
  setdiff(names(new), names(old))
}


#' Perform pivots
#'
#' @author Daniel Braby
#'
#' @description Bespoke pivot longer for Nursery Survey returns
#'
#' @param dir_path Directory path.
#' @param regex Matching criteria.
#'
#' @importFrom fs dir_info
#'
#' @return File paths.
#' @export


with_pivot <- function(x, names_from, values_from, action) {
  x %>%
    pivot_wider(names_from = all_of(names_from), values_from = all_of(values_from)) %>%
    action() %>%
    pivot_longer(names_diff(., x), names_to = names_from, values_to = values_from,
                 names_sep = switch(length(names_from) > 1, "_", NA),
                 names_transform = function(x) type.convert(x, as.is = TRUE))
}
