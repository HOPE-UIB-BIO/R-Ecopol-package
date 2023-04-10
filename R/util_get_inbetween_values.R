#' @title Get in-between values
#' @param data_source Vector with values
#' @param sel_output
#' Character. Define what should the function output.
#' \itemize{
#' \item `"only_inbetween"` - only inbetween values,
#' \item `"both"` - both inbetween values and original data
#' }
#' @description Add values in-between each pair or values in a vector. Position
#' of each value is calculated as average of 2 original values.
#' @keywords internal
util_get_inbetween_values <-
  function(data_source,
           sel_output = c("only_inbetween", "both")) {
    RUtilpol::check_class("data_source", "numeric")

    RUtilpol::check_class("sel_output", "character")

    sel_output <- match.arg(sel_output)

    RUtilpol::check_vector_values("sel_output", c("only_inbetween", "both"))

    # helper function
    offdiag <-
      function(matrix, offset) {
        s <- seq(offset)
        diag(matrix[, -s, drop = FALSE])
      }

    inbetween_values <-
      expand.grid(
        a = data_source,
        b = data_source
      ) %>%
      dplyr::mutate(
        avg = (a + b) / 2
      ) %>%
      tidyr::pivot_wider(names_from = b, values_from = avg) %>%
      tibble::column_to_rownames("a") %>%
      as.matrix() %>%
      offdiag(., 1)

    if (
      sel_output == "both"
    ) {
      c(
        data_source,
        inbetween_values
      ) %>%
        sort() %>%
        return()
    } else if (
      sel_output == "only_inbetween"
    ) {
      inbetween_values %>%
        sort() %>%
        return()
    }
  }
