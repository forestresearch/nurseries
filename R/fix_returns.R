#' Fix the returns that do not pass the validation test.
#' 
#' @author Daniel Braby
#' 
#'
#' @description Fixes the disaggregation errors.
#'
#' @param returns The raw returns.
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr reframe
#' @importFrom dplyr pick
#' @importFrom dplyr everything
#' @importFrom dplyr inner_join
#' 
#' 
#' @export



fix_returns <- function(raw_returns, nursery_names) {
  fix_disaggregation_error <- function(df) {
    disaggregation <- df %>% filter(country_sold_to == "E&W", !gi) %>% pull(volume)
    df %>% mutate(volume = volume + disaggregation *
                    ifelse(gi, -1, 1) *
                    ifelse(country_sold_to == "E&W", -1, 1) *
                    ifelse(disaggregation < 0, 1, 0))
  }

  raw_returns %>%
    reframe(fix_disaggregation_error(pick(everything())),
            .by = c(nursery_ref, tree_sp, prod_method, year)) %>%
    select(!nursery_name) %>%
    inner_join(nursery_names, by = "nursery_ref")
}

