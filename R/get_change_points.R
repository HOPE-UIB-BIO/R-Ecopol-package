#' @title Detect change points in a vector
#' @param data_source Vector with values (numeric, character, or factor)
#' @param direction 
#' Which direction should the values be compared ("front" or "back")
#' @return Vector with binary values of length = `data_source` + 1.
#' `1` represent change point
#' @export
get_change_points <-
  function(data_source,
           direction = c("front", "back")) {

    RUtilpol::check_class("data_source", c("numeric", "character", "factor"))

    RUtilpol::check_class("direction", "character")

    direction <- match.arg(direction)

    RUtilpol::check_vector_values("direction", c("front", "back"))

    if (
      class(data_source) %in% c("character", "factor")
    ) {
      data_numeric <-
        data_source %>%
        as.factor() %>%
        as.numeric()
    } else {
      data_numeric <-
        data_source %>%
        as.numeric()
    }

    if (
      direction == "front"
    ) {
      delta <-
        c(FALSE, (diff(data_numeric) != 0)) * 1
    } else if (
      direction == "back"
    ) {
      delta <-
        c((diff(data_numeric) != 0), FALSE) * 1
    }

    return(delta)
  }
