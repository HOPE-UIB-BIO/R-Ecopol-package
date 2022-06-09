#' @title Get in-between values
#' @param data_source Vector with values
#' @description Add values in-between each pair or values in a vector. Position
#' of each value is calculated as average of 2 original values.
util_get_inbetween_values <-
  function(data_source) {

    util_check_class("data_source", "numeric")

    # helper function
    offdiag <-
      function(matrix, offset) {
        s <- seq(offset)
        diag(matrix[ ,-s, drop = FALSE])
      }

    inbetween_values <-
      expand.grid(
        a = data_source,
        b = data_source) %>%
      dplyr::mutate(
        avg = (a + b)/2
      ) %>%
      tidyr::pivot_wider(names_from = b, values_from = avg) %>%
      tibble::column_to_rownames("a") %>%
      as.matrix() %>%
      offdiag(., 1)

    c(
      data_source,
      inbetween_values
    ) %>%
      sort() %>%
      return()
  }
