#' @title Fit single term hierarchical GAM model
#' @param y_var Character. Name of the Y-variable
#' @param x_var Character. Name of the X-variable
#' @param group_var Character. Name of grouping variable
#' @param error_family Character. Name of the error-family to be used
#' @param smooth_basis Character. Name of the smooth basis to use for `x_var`
#' @param data_source Data.frame with columns whose names are set by `y_var`,
#'  `x_var`, `group_var`
#' @param sel_k Numeric. Define `k` (wiggliness) for `x_var`
#' @param sel_m Numeric. User specify the order of the penalty for this term.
#' if `NULL`, function will use `1` or `2` depending on the presence of common
#' trend.
#' @param common_trend Logical. Should hGAM have a common shared trend?
#' @param use_parallel Logical. Should computation use parallel?
#' @param max_itiration Numeric. Maximum number of iteration for hGAM to try.
#' @description Fit a hierarchical GAM model with/without a single common
#' smoother (`common_trend`) plus group-level smoothers with differing
#' wiggliness (random effect). If there is less number of groups in the dataset
#' than 2, normal GAM model will be fitted.
#' If `use_parallel` is `TRUE`, number of cores is automatically detected.
#' @return Fitted hGAM model
#' @export
fit_hgam <-
  function(x_var = "age",
           y_var = "var",
           group_var = "dataset_id",
           error_family = "gaussian(link = 'identity')",
           smooth_basis = c("tp", "cr"),
           data_source,
           sel_k = 10,
           sel_m = NULL,
           common_trend = TRUE,
           use_parallel = TRUE,
           max_itiration = 200) {
    util_check_class("y_var", "character")

    util_check_class("x_var", "character")

    util_check_class("group_var", "character")

    util_check_class("error_family", "character")

    smooth_basis <- match.arg(smooth_basis)

    util_check_class("smooth_basis", "character")

    util_check_vector_values("smooth_basis", c("tp", "cr"))

    util_check_class("data_source", "data.frame")

    util_check_col_names("data_source", c(eval(group_var), eval(y_var), eval(x_var)))

    util_check_class("sel_k", "numeric")

    assertthat::assert_that(
      round(sel_k) == sel_k,
      msg = "'sel_k' must be an integer"
    )

    util_check_class("sel_m", c("NULL", "numeric"))

    if (
      is.null(sel_m) == FALSE
    ) {
      assertthat::assert_that(
        round(sel_m) == sel_m,
        msg = "'sel_m' must be an integer"
      )
    }

    util_check_class("common_trend", "logical")

    util_check_class("use_parallel", "logical")

    util_check_class("max_itiration", "numeric")

    assertthat::assert_that(
      round(max_itiration) == max_itiration,
      msg = "'max_itiration' must be an integer"
    )

    if (
      is.null(sel_m)
    ) {
      sel_m <-
        ifelse(common_trend, 1, 2)
    }

    current_env <- environment()

    data_source <-
      data_source %>%
      dplyr::mutate(!!group_var := as.factor(get(group_var)))

    n_groups <-
      data_source %>%
      dplyr::distinct(get(group_var)) %>%
      purrr::pluck(1) %>%
      length()

    cat(
      paste("N datasets:", n_groups), "\n"
    )

    formula_gam <-
      paste0(y_var, " ~ s(", x_var, ", k = ", sel_k, ", bs = '", smooth_basis, "')")

    formula_hgam <-
      paste(
        paste0("s(", x_var, ", by = ", group_var, ", bs = '", smooth_basis, "', m = ", sel_m, ")"),
        paste0("s(", group_var, ", bs = 're', k = ", n_groups, ")"),
        sep = " + "
      )

    if (
      common_trend == TRUE
    ) {
      formula_hgam_fin <-
        paste(
          formula_gam,
          formula_hgam,
          sep = " + "
        )
    } else {
      formula_hgam_fin <-
        paste0(y_var, " ~ ", formula_hgam)
    }

    if (
      n_groups > 2
    ) {
      cl <- NULL

      if (
        use_parallel == TRUE
      ) {
        number_of_cores <- parallel::detectCores()

        if (
          number_of_cores > n_groups
        ) {
          number_of_cores <- n_groups
        }

        cl <- parallel::makeCluster(number_of_cores)
      }

      try(
        fin_mod <-
          mgcv::bam(
            formula = stats::as.formula(formula_hgam_fin),
            data = data_source,
            method = "fREML",
            family = eval(parse(text = error_family)),
            cluster = cl,
            control = mgcv::gam.control(
              trace = TRUE,
              maxit = max_itiration
            )
          )
      )

      # close cluster
      if (
        !is.null(cl)
      ) {
        parallel::stopCluster(cl)
        cl <- NULL
      }
      gc(verbose = FALSE)
    } else {
      try(
        fin_mod <-
          mgcv::bam(
            formula = stats::as.formula(formula_gam),
            data = data_source,
            method = "fREML",
            family = eval(parse(text = error_family)),
            control = mgcv::gam.control(
              trace = TRUE,
              maxit = max_itiration
            )
          )
      )
    }

    if (!exists("fin_mod", envir = current_env)) {
      fin_mod <- NA_real_
    }

    return(fin_mod)
  }
