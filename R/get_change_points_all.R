#' @title Detect change points for all columns
#' @param data_source Data.frame with columns to detect change points in
#' @param var_name_order Character. Name of the columns, which should be use
#' as a order of values
#' @param use_inbetween_values Logical. If `TRUE` additional values will be placed
#' in between values of `var_name_order`.
#' @param var_name_detection Character. Columns to use to change point detection
#' will be automatically selected if they partly matched `var_name_detection`
#' @param direction Which direction should the values be compared ("front" or "back")
#' @return Data.frame with `var_name_order` and all columns detected based on
#' `var_name_detection`, with `1` represent change points.
#' @export
get_change_points_all <-
  function(data_source,
           var_name_order = "age",
           use_inbetween_values = TRUE,
           var_name_detection = "_part",
           direction = c("front", "back")) {

    util_check_class("data_source", "data.frame")

    util_check_class("var_name_order", "character")

    util_check_class("var_name_detection", "character")

    util_check_class("direction", "character")

    direction <- match.arg(direction)

    util_check_vector_values("direction", c("front", "back"))

    util_check_col_names("data_source", c(var_name_order, "sample_id"))

    assertthat::assert_that(
      stringr::str_detect(names(data_source), var_name_detection) %>%
        any(),
      msg = paste(
        "'data_source' must contain at least one column which partly matched with",
        var_name_detection
      )
    )

    if (
      use_inbetween_values == TRUE
    ) {
      data_position <-
        data_source %>%
        purrr::pluck(var_name_order) %>%
        util_get_inbetween_values()
    } else {

      data_position <-
        data_source %>%
        purrr::pluck(var_name_order)
    }

    var_list <-
      data_source %>%
      dplyr::select(
        dplyr::contains(
          c("sample_id", var_name_order, var_name_detection))
      ) %>%
      tidyr::pivot_longer(
        cols = !dplyr::all_of(c("sample_id", var_name_order)),
        names_to = "var",
        values_to = "value") %>%
      split(.$var)

    purrr::map_dfr(
      .x = var_list,
      .f = ~ tibble::tibble(
        !!var_name_order := data_position
      ) %>%
        dplyr::left_join(
          .,
          .x,
          by = var_name_order) %>%
        tidyr::fill(
          value,
          .direction = switch(direction,
                              "front" = "up",
                              "back" = "down")) %>%
        dplyr::mutate(
          cp = get_change_points(
            as.numeric(value),
            direction = direction)) %>%
        dplyr::select(
          dplyr::all_of(
            c(var_name_order, "cp")
          )
        ),
      .id = "var") %>%
      tidyr::pivot_wider(
        names_from = "var",
        values_from = "cp") %>%
      return()
  }
