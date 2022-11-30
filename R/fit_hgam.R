#' @title Fit single term hierarchical GAM model
#' @inheritParams fit_custom_gam
#' @inheritParams fit_gam_safely
#' @inheritParams fit_multiple_gams
#' @param sel_m Numeric. User specify the order of the penalty for this term.
#' if `NULL`, function will use `1` or `2` depending on the presence of common
#' trend.
#' @param common_trend Logical. Should hGAM have a common shared trend?
#' @param use_parallel Logical. Should computation use parallel?
#' @param use_discrete Logical. Should `discrete` agument be used for parallel
#' computation?
#' @description Fit a hierarchical GAM model with/without a single common
#' smoother (`common_trend`) plus group-level smoothers with differing
#' wiggliness (random effect). If there is less number of groups in the dataset
#' than 2, normal GAM model will be fitted.
#' If `use_parallel` is `TRUE`, number of cores is automatically detected.
#' @return Fitted hGAM model
#' @seealso {mgcv::bam()}
#' @export
fit_hgam <-
  function(x_var = "age",
           y_var = "var",
           group_var = "dataset_id",
           error_family = "gaussian(link = 'identity')",
           weights_var = NULL,
           smooth_basis = c("tp", "cr"),
           data_source,
           sel_k = 10,
           sel_m = NULL,
           common_trend = TRUE,
           use_parallel = TRUE,
           use_discrete = FALSE,
           max_iterations = 200,
           verbose = TRUE) {
    RUtilpol::check_class("y_var", "character")

    RUtilpol::check_class("x_var", "character")

    RUtilpol::check_class("group_var", "character")

    RUtilpol::check_class("error_family", "character")

    smooth_basis <- match.arg(smooth_basis)

    RUtilpol::check_class("smooth_basis", "character")

    RUtilpol::check_vector_values("smooth_basis", c("tp", "cr"))

    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names(
      "data_source",
      c(eval(group_var), eval(y_var), eval(x_var))
    )

    RUtilpol::check_class("sel_k", "numeric")

    assertthat::assert_that(
      round(sel_k) == sel_k,
      msg = "'sel_k' must be an integer"
    )

    RUtilpol::check_class("sel_m", c("NULL", "numeric"))

    if (
      is.null(sel_m) == FALSE
    ) {
      assertthat::assert_that(
        round(sel_m) == sel_m,
        msg = "'sel_m' must be an integer"
      )
    }

    RUtilpol::check_class("common_trend", "logical")

    RUtilpol::check_class("use_parallel", "logical")

    RUtilpol::check_class("use_discrete", "logical")

    if (
      use_parallel == FALSE
    ) {
      use_discrete <- FALSE
    }

    RUtilpol::check_class("max_iterations", "numeric")

    assertthat::assert_that(
      round(max_iterations) == max_iterations,
      msg = "'max_iterations' must be an integer"
    )

    RUtilpol::check_class("verbose", "logical")

    if (
      is.null(sel_m)
    ) {
      sel_m <-
        ifelse(common_trend, 1, 2)
    }

    if (is.null(weights_var) == FALSE) {
      RUtilpol::check_class("weights_var", "character")

      RUtilpol::check_col_names("data_source", eval(weights_var))

      data_source <-
        data_source %>%
        dplyr::mutate(weights = with(data_source, get(weights_var)))
    } else {
      data_source <-
        data_source %>%
        dplyr::mutate(weights = rep(1, nrow(data_source)))
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

    if (
      isTRUE(verbose)
    ) {
      RUtilpol::output_comment(
        paste("N datasets:", n_groups)
      )
    }

    formula_gam <-
      paste0(
        y_var,
        " ~ s(",
        x_var,
        ", k = ", sel_k,
        ", bs = '", smooth_basis, "'",
        ")"
      )

    formula_hgam <-
      paste(
        paste0(
          "s(", x_var,
          ", by = ", group_var,
          ", bs = '", smooth_basis, "'",
          ", k = ", sel_k,
          ", m = ", sel_m,
          ")"
        ),
        paste0(
          "s(", group_var,
          ", bs = 're'",
          ", k = ", n_groups,
          ")"
        ),
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

        if (
          isTRUE(verbose)
        ) {
          RUtilpol::output_comment(
            paste(
              "Using parallel estimation using N cores = ",
              number_of_cores
            )
          )
        }

        if (
          use_discrete == FALSE
        ) {
          sel_cluster_type <-
            ifelse(
              .Platform["OS.type"] == "unix",
              "FORK",
              "PSOCK"
            )

          cl <-
            parallel::makeCluster(
              number_of_cores,
              type = sel_cluster_type
            )

          try(
            fin_mod <-
              mgcv::bam(
                formula = stats::as.formula(formula_hgam_fin),
                data = data_source,
                weights = weights,
                method = "fREML",
                family = eval(parse(text = error_family)),
                cluster = cl,
                control = mgcv::gam.control(
                  trace = verbose,
                  maxit = max_iterations
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
                formula = stats::as.formula(formula_hgam_fin),
                data = data_source,
                weights = weights,
                method = "fREML",
                family = eval(parse(text = error_family)),
                discrete = TRUE,
                control = mgcv::gam.control(
                  trace = verbose,
                  maxit = max_iterations,
                  nthreads = number_of_cores
                )
              )
          )
        }
      } else {
        try(
          fin_mod <-
            mgcv::bam(
              formula = stats::as.formula(formula_hgam_fin),
              data = data_source,
              weights = weights,
              method = "fREML",
              family = eval(parse(text = error_family)),
              control = mgcv::gam.control(
                trace = verbose,
                maxit = max_iterations
              )
            )
        )
      }
    } else {
      if (
        isTRUE(verbose)
      ) {
        RUtilpol::output_comment(
          "Not enough groups to fit hGAM. Fitting GAM instead"
        )
      }

      try(
        fin_mod <-
          mgcv::bam(
            formula = stats::as.formula(formula_gam),
            data = data_source,
            weights = weights,
            method = "fREML",
            family = eval(parse(text = error_family)),
            control = mgcv::gam.control(
              trace = verbose,
              maxit = max_iterations
            )
          )
      )
    }

    if (
      !exists("fin_mod", envir = current_env)
    ) {
      fin_mod <- NA_real_
    }

    return(fin_mod)
  }
