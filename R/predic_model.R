#' Predict data from a regression model
#'
#' @param data_source Data.frame with values to be predicted upon
#' @param model_source Selected model. Can be any of supported models by
#' `marginaleffects`
#' @param exclude_var Vector with terms to exclude. This is used only for `gam`
#' models
#' @description Predict model using `marginaleffects::predictions` function
#' @export
predic_model <-
  function(data_source, model_source, exclude_var = NULL) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_class("exclude_var", c("character", "NULL"))

    if (
      "gam" %in% class(model_source)
    ) {
      raw_pred <-
        marginaleffects::predictions(
          model = model_source,
          newdata = data_source,
          exclude = exclude_var
        )
    } else {
      raw_pred <-
        marginaleffects::predictions(
          model = model_source,
          newdata = data_source
        )
    }

    raw_pred %>%
      dplyr::rename(
        fit = predicted,
        lwr = conf.low,
        upr = conf.high,
        sd_error = std.error
      ) %>%
      dplyr::select(
        dplyr::any_of(
          c(
            names(data_source),
            "fit",
            "sd_error",
            "lwr",
            "upr"
          )
        )
      ) %>%
      tibble::as_tibble() %>%
      return()
  }
