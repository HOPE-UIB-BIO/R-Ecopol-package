#' @title Get density from a vector of numbers
#' @param data_source Vector with numeric values
#' @param values_range A numeric vector of length 2 specifying the range to
#' rescale the data to. If missing, the range of the data is used.
#' @param reflected Logical. Whether or not to reflect the data around 0?
#' (from Density Estimation for Statistics and Data Analysis by B.W.Silverman)
#' @param ... Additional parameters used in `stats::density`
#' @description
#' This function calculates the density of a numeric vector, potentially
#' rescaling it to a specific range and reflecting it. it is a wrapper for
#' `stats::density()`.
#' @return Data.frame with the `data_source` values and estimated density+
#' @seealso [stats::density()]
#' @export
get_density <-
  function(data_source,
           values_range,
           reflected = TRUE,
           ...) {
    RUtilpol::check_class("data_source", "numeric")

    if (
      missing(values_range)
    ) {
      values_range <- range(data_source)
    }

    RUtilpol::check_class("values_range", "numeric")

    assertthat::assert_that(
      length(values_range) == 2,
      msg = paste(
        "'values_range' must have 2 values"
      )
    )

    RUtilpol::check_class("reflected", "logical")

    rescale_to <- c(0, 1)

    data_rescaled <-
      scales::rescale(data_source, from = values_range, to = rescale_to)

    if (
      reflected == TRUE
    ) {
      data_work <-
        c(-(data_rescaled), data_rescaled, (2 - (data_rescaled)))
    } else {
      data_work <- data_rescaled
    }

    density_values <-
      stats::density(
        x = data_work,
        from = min(rescale_to),
        to = max(rescale_to),
        kernel = "gaussian",
        ...
      )

    tibble::tibble(
      x = density_values$x,
      y = density_values$y * ifelse(reflected, 3, 1)
    ) %>%
      dplyr::mutate(
        var = scales::rescale(x, from = rescale_to, to = values_range)
      ) %>%
      dplyr::select(var, density = y) %>%
      return()
  }
