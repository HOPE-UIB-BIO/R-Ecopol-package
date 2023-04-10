#' @title Fit multiple individual generalized additive models (GAMs)
#' @inheritParams fit_custom_gam
#' @inheritParams fit_gam_safely
#' @param data_source Data frame with `x_var`, `y_var`, and `group_var`
#' @param group_var Character. Name of the variable used for identification of
#' groups.
#' @description 
#' This function fits multiple generalized additive models (GAMs) using the
#' \code{fit_gam_safely()} function. GAMs are fitted
#' to each dataset defined by the \code{group_var} argument in the input
#' \code{data_source} data frame. The number of models fitted is equal to the
#' number of unique levels of the \code{group_var}. The function returns a data
#' frame with one row per group, including the original group identifier and the
#' fitted GAM model for that group.
#' @return A data frame containing the original grouping variable and the
#' corresponding fitted GAM model for each group.
#' @seealso [fit_gam_safely()]
fit_multiple_gams <-
  function(data_source,
           x_var = "age",
           y_var = "var",
           group_var = "dataset_id",
           smooth_basis = c("tp", "cr"),
           error_family = "gaussian(link = 'identity')",
           weights_var = NULL,
           max_k = 10,
           max_iterations = 200,
           verbose = TRUE) {
    RUtilpol::check_class("y_var", "character")

    RUtilpol::check_class("x_var", "character")

    RUtilpol::check_class("error_family", "character")

    RUtilpol::check_class("smooth_basis", "character")

    RUtilpol::check_vector_values("smooth_basis", c("tp", "cr"))

    smooth_basis <- match.arg(smooth_basis)

    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names(
      "data_source",
      c(
        eval(y_var),
        eval(x_var),
        eval(group_var)
      )
    )

    RUtilpol::check_class(
      "weights_var",
      c(
        "character",
        "NULL"
      )
    )

    assertthat::assert_that(
      round(max_iterations) == max_iterations,
      msg = "'max_iterations' must be an integer"
    )

    RUtilpol::check_class("verbose", "logical")

    if (
      isTRUE(verbose)
    ) {
      RUtilpol::output_comment("fitting multiple GAMs")
    }

    n_datasets <-
      data_source %>%
      dplyr::distinct(get(group_var)) %>%
      purrr::pluck(1) %>%
      length()

    if (
      isTRUE(verbose)
    ) {
      RUtilpol::output_comment(
        paste("N datasets:", n_datasets)
      )
    }

    suppressWarnings(
      res <-
        data_source %>%
        dplyr::select(
          dplyr::all_of(
            c(group_var, x_var, y_var, weights_var)
          )
        ) %>%
        dplyr::group_by(get(group_var)) %>%
        tidyr::nest(data = dplyr::any_of(
          c(
            x_var,
            y_var,
            weights_var
          )
        )) %>%
        tidyr::drop_na(data) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          n_levels = purrr::map_dbl(
            .x = data,
            .f = nrow
          )
        ) %>%
        dplyr::mutate(
          mod = purrr::pmap(
            .l = list(data, get(group_var), n_levels),
            .f = ~ {
              if (
                isTRUE(verbose)
              ) {
                message(..2)
              }

              fit_gam_safely(
                data_source = ..1,
                x_var = x_var,
                y_var = y_var,
                smooth_basis = smooth_basis,
                error_family = error_family,
                sel_k = (..3 - 1),
                max_k = max_k,
                weights_var = weights_var,
                max_iterations = max_iterations,
                verbose = verbose
              ) %>%
                return()
            }
          )
        )
    )

    return(res)
  }
