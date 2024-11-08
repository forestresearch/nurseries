#' Read all descendant spreadsheets in a folder and bind the reports together.
#'
#' @author Dan Braby
#'
#' @description
#' Reads in all Nursery Survey returns in a specified spreadsheet and compiles into a single dataframe
#'
#' @param dir_path Path to search for spreadsheets.
#'
#' @importFrom purrr map_dfr
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr recode
#' 
#' @return Combined tibble of reports.
#' @export
#'
read_returns <- function(dir_path) {
  dir_path %>%
    files_matching("\\.xlsx$") %>%
    purrr::map_dfr(read_return)
}

#' Read a single nursery survey return from a spreadsheet.
#'
#' @return Tibble with disaggregated country and genetically improved stock variables.
#' @export
#'
read_return <- function(file_path) {
  invisible(readxl::read_xlsx(path = file_path, sheet = "return")) %>%
    with_pivot("country_sold_to", "volume_thousand",
               function (x) {
                 x %>% mutate("E&W" = GB - Scotland) %>%
                   select(!GB)
               }) %>%
    with_pivot("stock_type", "volume_thousand",
               function (x) {
                 x %>% mutate("Non-GI" = Total - GI) %>%
                   select(!Total)
               }) %>%
    mutate(
      gi = recode(stock_type, "GI" = TRUE, "Non-GI" = FALSE),
      year = as.integer(year),
      nursery_ref = as.integer(nursery_ref),
      volume = volume_thousand * 1000,
      .keep = "unused"
    )
}
