#' @title  Re-number cluster groups
#' @param clusgr Vector with integers
#' @description Utility function to re-number vector of integers to simple
#' sequence of numbers
#' @keywords internal
util_renumber_groups <-
  function(clusgr) {
    as.numeric(
      as.factor(clusgr)
    ) %>%
      return()
  }
