#' @title Detect change points for all columns
#' @param data_source Data.frame with columns to detect change points in
#' @param var_name_order Character.
#' Name of the columns, which should be use as a order of values
#' @param use_inbetween_values
#' Logical. If `TRUE` additional values will be placed
#' in between values of `var_name_order`.
#' #' @param sel_output
#' Character. Only used for `use_inbetween_values` == `TRUE`
#' Define what should the function output.
#' \itemize{
#' \item `"only_inbetween"` - only inbetween values,
#' \item `"both"` - both inbetween values and original data
#' }
#' @param var_name_detection
#' Character. Optional. Columns to use to change point detection
#' will be automatically selected if they partly matched `var_name_detection`.
#' For all columns use `NULL``.
#' @param direction
#' Which direction should the values be compared ("front" or "back")
#' @return Data.frame with `var_name_order` and all columns detected based on
#' `var_name_detection`, with `1` represent change points.
#' @export
get_change_points_all <-
  function(data_source,
           var_name_order = "age",
           use_inbetween_values = TRUE,
           sel_output = c("only_inbetween", "both"),
           var_name_detection = NULL,
           direction = c("front", "back")) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_class("var_name_order", "character")

    RUtilpol::check_class("use_inbetween_values", "logical")

    RUtilpol::check_class("direction", "character")

    direction <- match.arg(direction)

    RUtilpol::check_vector_values("direction", c("front", "back"))

    RUtilpol::check_col_names("data_source", c(var_name_order, "sample_id"))

    if (
      use_inbetween_values == TRUE
    ) {
      RUtilpol::check_class("sel_output", "character")

      sel_output <- match.arg(sel_output)

      RUtilpol::check_vector_values("sel_output", c("only_inbetween", "both"))

      data_position_work <-
        data_source %>%
        purrr::pluck(var_name_order) %>%
        util_get_inbetween_values(
          data_source = .,
          sel_output = "both"
        )

      data_position_fin <-
        data_source %>%
        purrr::pluck(var_name_order) %>%
        util_get_inbetween_values(
          data_source = .,
          sel_output = sel_output
        )
    } else {
      data_position_work <-
        data_position_fin <-
        data_source %>%
        purrr::pluck(var_name_order)
    }

    if
    (
      is.null(var_name_detection) == FALSE
    ) {
      RUtilpol::check_class("var_name_detection", "character")

      assertthat::assert_that(
        stringr::str_detect(names(data_source), var_name_detection) %>%
          any(),
        msg = paste(
          "'data_source' must contain at least one column",
          "which partly matched with",
          RUtilpol::paste_as_vector(var_name_detection)
        )
      )
    } else {
      var_name_detection <-
        data_source %>% 
        dplyr::select(
          !dplyr::any_of(
            c(
              "sample_id",
              var_name_order
            )
          )
        ) %>%
        names()
    }

    var_list <-
      data_source %>%
      dplyr::select(
        dplyr::contains(
          c("sample_id", var_name_order, var_name_detection)
        )
      ) %>%
      tidyr::pivot_longer(
        cols = !dplyr::all_of(c("sample_id", var_name_order)),
        names_to = "var",
        values_to = "value"
      ) %>%
      split(.$var)

    purrr::map_dfr(
      .x = var_list,
      .f = ~ tibble::tibble(
        !!var_name_order := data_position_work
      ) %>%
        dplyr::left_join(
          .,
          .x,
          by = var_name_order
        ) %>%
        tidyr::fill(
          value,
          .direction = switch(direction,
            "front" = "up",
            "back" = "down"
          )
        ) %>%
        dplyr::mutate(
          cp = get_change_points(
            as.numeric(value),
            direction = direction
          )
        ) %>%
        dplyr::select(
          dplyr::all_of(
            c(var_name_order, "cp")
          )
        ),
      .id = "var"
    ) %>%
      tidyr::pivot_wider(
        names_from = "var",
        values_from = "cp"
      ) %>%
      dplyr::filter(
        get(var_name_order) %in% data_position_fin
      ) %>%
      return()
  }
