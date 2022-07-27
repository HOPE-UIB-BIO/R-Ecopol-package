
#' @title Fit multiple individual GAM models
#' @param data_source Data frame with `x_var`, `y_var`, and `group_var`
#' @inheritParams fit_gam_safely
#' @description Helper function to apply multiple`fit_gam_safely`.
#' @seealso [fit_gam_safely()] 
fit_multiple_gams <-
  function(data_source,
           x_var = "age",
           y_var = "var",
           group_var = "dataset_id",
           smooth_basis = c("tp", "cr"),
           error_family = "gaussian(link = 'identity')",
           max_k = 10) {
    util_check_class("y_var", "character")

    util_check_class("x_var", "character")

    util_check_class("error_family", "character")

    util_check_class("smooth_basis", "character")

    util_check_vector_values("smooth_basis", c("tp", "cr"))

    smooth_basis <- match.arg(smooth_basis)

    util_check_class("data_source", "data.frame")

    util_check_col_names(
      "data_source",
      c(
        eval(y_var),
        eval(x_var),
        eval(group_var)
      )
    )

    cat("fitting multiple GAMs ")

    n_datasets <-
      data_source %>%
      dplyr::distinct(get(group_var)) %>%
      purrr::pluck(group_var) %>%
      length()

    cat(paste("N datasets:", n_datasets), "\n")

    suppressWarnings(
      res <-
        data_source %>%
        dplyr::group_by(get(group_var)) %>%
        tidyr::nest(data = c(eval(x_var), eval(y_var))) %>%
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
              message(..2)

              REcopol:::fit_gam_safely(
                data_source = ..1,
                x_var = x_var,
                y_var = y_var,
                smooth_basis = smooth_basis,
                error_family = error_family,
                sel_k = (..3 - 1),
                max_k = max_k
              ) %>%
                return()
            }
          )
        )
    )

    return(res)
  }