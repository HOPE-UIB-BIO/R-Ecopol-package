#' @title Safely fit single term GAM
#' @inheritParams fit_custom_gam
#' @param sel_k Preferred `k` (wiggliness)
#' @param max_k Maximum `k` which can be used
#' @description A wrapper function for `fit_custom_gam`. The `k`is compared to
#' the number of samples and to the `max_k`. If there is a zero variability in
#' the term, `lm` is fitted instead. If no model was cretaed (due to errors),
#' the function retun `NA_real`
#' @seealso [fit_custom_gam()]
#' @keywords internal
fit_gam_safely <-
  function(data_source,
           x_var = "age",
           y_var = "var",
           smooth_basis = c("tp", "cr"),
           error_family = "gaussian(link = 'identity')",
           weights_var = NULL,
           sel_k = 10,
           max_k = 10,
           max_iterations = 200,
           verbose = FALSE) {
    current_env <- environment()

    RUtilpol::check_class("y_var", "character")

    RUtilpol::check_class("x_var", "character")

    RUtilpol::check_class("error_family", "character")

    smooth_basis <- match.arg(smooth_basis)

    RUtilpol::check_class("smooth_basis", "character")

    RUtilpol::check_vector_values("smooth_basis", c("tp", "cr"))

    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", c(eval(y_var), eval(x_var)))

    RUtilpol::check_class(
      "weights_var",
      c(
        "character",
        "NULL"
      )
    )

    RUtilpol::check_class("sel_k", "numeric")

    assertthat::assert_that(
      round(sel_k) == sel_k,
      msg = "'sel_k' must be an integer"
    )

    RUtilpol::check_class("max_k", "numeric")

    assertthat::assert_that(
      round(max_k) == max_k,
      msg = "'max_k' must be an integer"
    )

    RUtilpol::check_class("max_iterations", "numeric")

    assertthat::assert_that(
      round(max_iterations) == max_iterations,
      msg = "'max_iterations' must be an integer"
    )

    RUtilpol::check_class("verbose", "logical")

    if (
      sel_k > nrow(data_source) - 1
    ) {
      sel_k <- nrow(data_source) - 1
    }

    # if the selected `sel_k` is bigger than maximum `max_k`, use the `max_k`
    if (sel_k >= max_k) {
      sel_k <- max_k
    }

    # transform into numeric
    data_source <-
      data_source %>%
      dplyr::mutate(
        !!y_var := as.numeric(get(y_var))
      )

    # if there is no variation in the var, use `lm` (straith line)
    if (
      data_source %>%
        purrr::pluck(y_var) %>%
        sd() == 0
    ) {
      if (
        isTRUE(verbose)
      ) {
        RUtilpol::output_comment(
          paste(
            "Detected no changes in", eval(y_var),
            "Fittin 'lm' instead"
          )
        )
      }

      formula_w <-
        paste0(y_var, "~", x_var) %>%
        stats::as.formula(.)

      fin_mod <-
        stats::lm(
          formula = formula_w,
          data = data_source
        )
    } else {
      try(
        fin_mod <-
          fit_custom_gam(
            x_var = x_var,
            y_var = y_var,
            error_family = error_family,
            smooth_basis = smooth_basis,
            data_source = data_source,
            weights_var = weights_var,
            sel_k = sel_k,
            max_iterations = max_iterations,
            verbose = verbose
          ),
        silent = verbose
      )
    }

    ifelse(
      exists("fin_mod", envir = current_env),
      return(fin_mod),
      return(NA_real_)
    )
  }
