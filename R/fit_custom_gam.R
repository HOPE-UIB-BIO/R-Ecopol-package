#' @title Fit custom single term GAM model
#' @param y_var Character. Name of the Y-variable
#' @param x_var Character. Name of the X-variable
#' @param error_family Character. Name of the error-family to be used
#' @param smooth_basis Character. Name of the Smooth basis to use
#' @param data_source Data.frame
#' @param weights_var Character. Name of the variable to use as weights
#' @param sel_k Define `k` (wiggliness)
#' @return Fitted GAM model
#' @export
fit_custom_gam <-
  function(
    y_var,
    x_var,
    error_family,
    smooth_basis = c('tp', 'cr'),
    data_source,
    weights_var = NULL,
    sel_k = 10
  ){

    util_check_class("y_var", "character")

    util_check_class("x_var", "character")

    util_check_class("error_family", "character")

    smooth_basis <- match.arg(smooth_basis)

    util_check_class("smooth_basis", "character")

    util_check_vector_values("smooth_basis", c('tp', 'cr'))

    util_check_class("data_source", "data.frame")

    util_check_col_names("data_source", c(eval(y_var), eval(x_var)))

    if(is.null(weights_var) == FALSE){

      util_check_class("weights_var", "character")

      util_check_col_names("data_source", eval(weights_var))

      data_weight <-
        data_source %>%
        dplyr::mutate(weights = with(data_source, get(weights_var)))


    } else {

      data_weight <-
        data_source %>%
        dplyr::mutate(weights = rep(1, nrow(data_source)))

    }

    util_check_class("sel_k", "numeric")

    assertthat::assert_that(
      round(sel_k) == sel_k,
      msg = "'sel_k' must be an integer")

    if(
      sel_k > nrow(data_weight)-1
    ) {
      sel_k <- nrow(data_weight)-1
    }

    formula_w <-
      paste0(y_var, "~ s(", x_var,", k = ", sel_k, ",bs = '", smooth_basis, "' )") %>%
      stats::as.formula(.)

    res_gam <-
      mgcv::gam(
        formula = formula_w,
        data = as.data.frame(data_weight),
        family =  eval(parse(text = error_family)),
        weights = weights,
        method = "REML",
        na.action = "na.omit")

    return(res_gam)
  }
