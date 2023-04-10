#' @title Fit DCCA using CANOCO
#' @description
#' Fits a DCCA using CANOCO.exe program
#' @param data_source_resp
#' data frame with response data (pollen counts or percentages). Sample ID as
#' rownames.
#' @param data_source_pred
#' Data frame with the predictor values (depth or age).  Sample ID as
#' rownames.
#' The number of rows must be identical with the
#' number of rows in `data_source_resp`
#' @param sel_complexity
#' Indicate how the predictor should be used
#' \itemize{
#' \item `"linear"` - a linear predictor
#' \item `"poly_2"` - a polynomial predictor of second order
#' \item `"poly_3"` - a polynomial predictor of third order
#' }
#' @param downweight
#' logical, whether to downweight rare species or not
#' @return
#' \itemize{
#' \item `case_r` - numeric matrix with CaseR scores for first 4 axes
#' \item `tot_inertia` - total variation in (transformed) response data
#' \item `sel_complexity` - see `sel_complexity` agument description
#' }
fit_ordination_dcca <-
  function(data_source_resp,
           data_source_pred,
           sel_complexity = c(
             "linear",
             "poly_2",
             "poly_3"
           ),
           downweight = FALSE) {
    RUtilpol::check_class("data_source_resp", "data.frame")

    RUtilpol::check_class("data_source_pred", "data.frame")

    RUtilpol::check_class("downweight", "logical")

    RUtilpol::check_class("sel_complexity", "character")

    RUtilpol::check_vector_values(
      "sel_complexity",
      c(
        "linear",
        "poly_2",
        "poly_3"
      )
    )

    sel_complexity <-
      match.arg(sel_complexity)

    assertthat::assert_that(
      all(row.names(data_source_resp) %in% row.names(data_source_pred)) &&
        all(row.names(data_source_pred) %in% row.names(data_source_resp)),
      msg = "'data_source_resp' and 'data_source_pred' must have same row.names"
    )

    pred_name <-
      names(data_source_pred)[1]

    pred_df <-
      data_source_pred[, 1, drop = FALSE]

    switch(sel_complexity,
      "linear" = {
        pred_df <-
          pred_df %>%
          rlang::set_names(
            nm = paste0(pred_name, "_linear")
          )
      },
      "poly_2" = {
        pred_df <-
          pred_df %>%
          dplyr::mutate(
            temp = NA
          ) %>%
          rlang::set_names(
            nm = c(
              paste0(pred_name, "_poly_1"),
              paste0(pred_name, "_poly_2")
            )
          )
        pred_df[, 1:2] <-
          stats::poly(pred_df[, 1], 2)
      },
      "poly_3" = {
        pred_df <-
          pred_df %>%
          dplyr::mutate(
            temp1 = NA,
            temp2 = NA
          ) %>%
          rlang::set_names(
            nm = c(
              paste0(pred_name, "_poly_1"),
              paste0(pred_name, "_poly_2"),
              paste0(pred_name, "_poly_3")
            )
          )
        pred_df[, 1:3] <-
          stats::poly(pred_df[, 1], 3)
      }
    )

    # Execute CANOCO
    can_res <-
      dcca_execute_canoco(
        data_resp = data_source_resp,
        data_pred = pred_df,
        downweight = downweight
      )

    case_r_formated <-
      can_res %>%
      purrr::pluck("case_r") %>%
      as.data.frame()  %>% 
      rlang::set_names(
        nm = c(
          paste0("axis_", 1:4)
        )
      ) %>%
      tibble::rownames_to_column("sample_id")  %>% 
      tibble::as_tibble()

    return(
      list(
        case_r = case_r_formated,
        tot_inertia = can_res %>%
          purrr::pluck("tot_inertia"),
        sel_complexity = sel_complexity
      )
    )
  }
