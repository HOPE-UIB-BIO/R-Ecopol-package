#' Predict data from a linear model
#'
#' @param data_source Data.frame with values to be predicted upon
#' @param model_source Selected model. Can be `lm`, `glm`, or `gam`
#' @param exclude_var Vector with terms to exclude. This is used only for `gam`
#' models
#' @export
predic_model <-
  function(data_source, model_source, exclude_var = NA){

    util_check_class("data_source", "data.frame")

    util_check_class("model_source", "lm")

    util_check_class("exclude_var", c("character", "logical"))

    crit <- qnorm((1 - 0.89) / 2, lower.tail = FALSE)

    data_source %>%
      dplyr::bind_cols(
        predict(
          model_source,
          newdata = data_source,
          type = "response",
          se.fit = TRUE,
          exclude = exclude_var)) %>%
      dplyr::mutate(
        var = fit,
        lwr = fit - (crit * se.fit),
        upr = fit + (crit * se.fit)) %>%
      dplyr::select(
        !dplyr::any_of(
          c("fit", "se.fit","df", "residual.scale")
        ))  %>%
      return()
  }
