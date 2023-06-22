#' @title Fit custom single-term GAM model
#' @param y_var Character. Name of the Y-variable
#' @param x_var Character. Name of the X-variable
#' @param error_family Character. Name of the error family to be used
#' @param smooth_basis Character. Name of the Smooth basis to use
#' @param data_source Data.frame with columns whose names are set by `y_var` and
#'  `x_var`
#' @param weights_var Character. Name of the variable to use as weights
#' @param sel_k Numeric. Define `k` (wiggliness)
#' @param max_iterations Numeric. Maximum number of iterations for GAM to try.
#' @param verbose Logical. Should additional messages be output?
#' @description A wrapper function for `mgcv::gam` to help fit GAM models
#' functionally.
#' @return Fitted GAM model
#' @seealso [mgcv::gam()]
#' @export
fit_custom_gam <-
  function(x_var = "age",
           y_var = "var",
           error_family = "gaussian(link = 'identity')",
           smooth_basis = c("tp", "cr"),
           data_source,
           weights_var = NULL,
           sel_k = 10,
           max_iterations = 200,
           verbose = FALSE) {
    RUtilpol::check_class("y_var", "character")

    RUtilpol::check_class("x_var", "character")

    RUtilpol::check_class("error_family", "character")

    smooth_basis <- match.arg(smooth_basis)

    RUtilpol::check_class("smooth_basis", "character")

    RUtilpol::check_vector_values("smooth_basis", c("tp", "cr"))

    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", c(eval(y_var), eval(x_var)))

    if (is.null(weights_var) == FALSE) {
      RUtilpol::check_class("weights_var", "character")

      RUtilpol::check_col_names("data_source", eval(weights_var))

      data_weight <-
        data_source %>%
        dplyr::mutate(weights = with(data_source, get(weights_var)))
    } else {
      data_weight <-
        data_source %>%
        dplyr::mutate(weights = rep(1, nrow(data_source)))
    }

    RUtilpol::check_class("sel_k", "numeric")

    assertthat::assert_that(
      round(sel_k) == sel_k,
      msg = "'sel_k' must be an integer"
    )

    RUtilpol::check_class("max_iterations", "numeric")

    assertthat::assert_that(
      round(max_iterations) == max_iterations,
      msg = "'max_iterations' must be an integer"
    )

    RUtilpol::check_class("verbose", "logical")

    formula_w <-
      paste0(
        y_var,
        "~ s(", x_var, ", k = ", sel_k, ",bs = '", smooth_basis, "' )"
      ) %>%
      stats::as.formula(.)

    res_gam <-
      mgcv::gam(
        formula = formula_w,
        data = as.data.frame(data_weight),
        family = eval(parse(text = error_family)),
        weights = weights,
        method = "REML",
        na.action = "na.omit",
        control = mgcv::gam.control(
          trace = verbose,
          maxit = max_iterations
        )
      )

    return(res_gam)
  }
