#' @title Fit single term hierarchical GAM model
#' @param y_var Character. Name of the Y-variable
#' @param x_var Character. Name of the X-variable
#' @param group_var Character. Name of grouping variable
#' @param error_family Character. Name of the error-family to be used
#' @param smooth_basis Character. Name of the smooth basis to use for `x_var`
#' @param data_source Data.frame with columns whose names are set by `y_var`,
#'  `x_var`, `group_var`
#' @param #' @param sel_k Numeric. Define `k` (wiggliness) for `x_var`
#' @param max_itiration Numeric. Maximum number of iteration for hGAM to try.
#' @description Fit a hierarchical GAM model with a single common smoother plus
#' group-level smoothers with differing wiggliness (random effect). If there is
#' less number of groups in the dataset than 2, normal GAM model will be fitted.
#' The function uses cluser analyses with automatically detectec number of cores.
#' @return Fitted hGAM model
#' @export
fit_hgam <-
  function(
    x_var = "age",
    y_var = "var",
    group_var = "dataset_id",
    error_family = "gaussian(link = 'identity')",
    smooth_basis = c('tp', 'cr'),
    data_source,
    sel_k = 10,
    max_itiration = 200) {

    util_check_class("y_var", "character")

    util_check_class("x_var", "character")

    util_check_class("group_var", "character")

    util_check_class("error_family", "character")

    smooth_basis <- match.arg(smooth_basis)

    util_check_class("smooth_basis", "character")

    util_check_vector_values("smooth_basis", c('tp', 'cr'))

    util_check_class("data_source", "data.frame")

    util_check_col_names("data_source", c(eval(group_var), eval(y_var), eval(x_var)))

    util_check_class("sel_k", "numeric")

    assertthat::assert_that(
      round(sel_k) == sel_k,
      msg = "'sel_k' must be an integer")

    util_check_class("max_itiration", "numeric")

    assertthat::assert_that(
      round(max_itiration) == max_itiration,
      msg = "'max_itiration' must be an integer")

    current_env <- environment()

    n_groups <-
      data_source %>%
      dplyr::distinct(get(group_var)) %>%
      purrr::pluck(1) %>%
      length()

    cat(
      paste("N datasets:", n_groups), "\n")

    formula_gam <-
      paste0(y_var, " ~ s(", x_var,", k = ", sel_k, ", bs = '", smooth_basis, "')")

    formula_hgam <-
      paste(
        formula_gam,
        paste0("s(", group_var, ", bs = 're')"),
        paste0("ti(", x_var,", ", group_var, ", bs = c('", smooth_basis, "', 're'))"),
        paste0("s(", x_var,", by = ", group_var, ", bs = '", smooth_basis, "', m = 1)"),
        sep = " + "
      )

    if (n_groups > 2) {

      number_of_cores <- parallel::detectCores() - 1

      if(number_of_cores > n_groups) {
        number_of_cores <- n_groups
      }

      cl <- parallel::makeCluster(number_of_cores)

      try(
        fin_mod <-
          mgcv::bam(
            formula = stats::as.formula(formula_hgam),
            data = data_source,
            method = "fREML",
            family = eval(parse(text = sel_var_error)),
            cluster = cl,
            control = mgcv::gam.control(
              trace = TRUE,
              maxit = max_itiration))
      )

      parallel::stopCluster(cl)

      rm(cl)

    } else {
      try(
        fin_mod <-
          mgcv::bam(
            formula = stats::as.formula(formula_gam),
            data = data_source,
            method = "fREML",
            family = eval(parse(text = sel_var_error)),
            control = mgcv::gam.control(
              trace = TRUE,
              maxit = max_itiration))
      )
    }

    if(!exists("fin_mod", envir = current_env)){
      fin_mod <- NA_real_
    }

    return(fin_mod)

  }
