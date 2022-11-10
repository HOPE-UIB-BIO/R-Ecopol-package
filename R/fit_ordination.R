#' @title Fit ordination
#' @param data_source_community
#' Data.frame with community data. Each row is sample. First columns is
#' `sample_id`, each other is taxa.
#' @param sel_method
#' \itemize{
#' \item `"unconstrained"` - Detrended Correspondence Analysis
#' \item `"constrained"` - Detrended Correspondence Canonical Analysis
#' }
#' @param data_source_predictors
#' Data.frame with predictors. First columns is `sample_id`. Other columns
#' can be predictors.
#' @param var_name_pred
#' Character. Vector with the name of predictot variable.
#' @param transform_to_percentage
#' Logical. Should community data be tranformed into percentages?
#' @param tranformation
#' Logical. Which tranformation should be applied?
#' \itemize{
#' \item "none" -  without transformation
#' \item "chisq" - Chisq tranformation
#' \item "hellinger" - Hellinger tranformation
#' }
#' It is recomneded to apply tranformation to percentage data.
#' @inheritParams fit_ordination_dcca
#' @description
#' This is a wrapper function to perform standard data preparation
#' and either unconstrained (DCA) or consatrined ordination (DCCA)
#' with fossil pollen data. By default constrained ordination is constrained by
#' `"age"`.
#' @returns
#' \itemize{
#' \item `case_r` - numeric matrix with CaseR scores for first 4 axes
#' \item `tot_inertia` - total variation in (transformed) response data
#' \item `axis_1_grad_length` - total gradient length of first axis
#' }
#' Addtional values are reported for `unconstrained`
#' \itemize{
#' \item `ordination` - ordination object of class "decorana"
#' }
#' Addtional values are reported for `constrained`
#' \itemize{
#' \item `sel_complexity` - see `sel_complexity` agument description
#' }
#' @export
fit_ordination <-
    function(data_source_community,
             sel_method = c("unconstrained", "constrained"),
             data_source_predictors = NULL,
             var_name_pred = "age",
             sel_complexity = c(
                 "linear",
                 "poly_2",
                 "poly_3"
             ),
             transform_to_percentage = FALSE,
             tranformation = c("none", "chisq", "hellinger")) {
        RUtilpol::check_class("data_source_community", "data.frame")

        RUtilpol::check_col_names("data_source_community", "sample_id")

        RUtilpol::check_class("data_source_predictors", c("data.frame", "NULL"))

        RUtilpol::check_class("sel_method", "character")

        RUtilpol::check_vector_values(
            "sel_method",
            c("unconstrained", "constrained")
        )

        sel_method <- match.arg(sel_method)

        RUtilpol::check_class("transform_to_percentage", "logical")

        RUtilpol::check_class("tranformation", "character")

        RUtilpol::check_vector_values(
            "tranformation",
            c("none", "chisq", "hellinger")
        )

        tranformation <- match.arg(tranformation)

        if (
            transform_to_percentage == FALSE &&
                tranformation != "none"
        ) {
            warning(
                paste(
                    "We recomend to apply the 'tranformation'",
                    " to a percentage data.",
                    "Consider setting 'transform_to_percentage == TRUE'"
                )
            )
        }

        # get the `sample_id` of community data
        community_sample_id <-
            data_source_community %>%
            purrr::pluck("sample_id")

        if (
            sel_method == "constrained"
        ) {
            RUtilpol::check_class("var_name_pred", "character")

            assertthat::assert_that(
                length(var_name_pred) == 1,
                msg = "The 'constrained' method can only handle single predictor"
            )

            RUtilpol::check_col_names(
                "data_source_predictors",
                c(
                    "sample_id",
                    var_name_pred
                )
            )

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

            # get the `sample_id` of predictor data
            pred_sample_id <-
                data_source_predictors %>%
                purrr::pluck("sample_id")

            # make sure that all `sample_id` match between datasets
            assertthat::assert_that(
                all(community_sample_id %in% pred_sample_id) &&
                    all(pred_sample_id %in% community_sample_id),
                msg = paste(
                    "'sample_id' column must contain the same values in both",
                    "'data_source_community' and 'data_source_predictors'."
                )
            )
        }

        # data tranformation
        if (
            transform_to_percentage == TRUE
        ) {
            data_source_community <-
                transfer_into_proportions(
                    data_source_community,
                    method = "percentages"
                )
        }

        data_com <-
            # use transformation (there is "none" by default)
            tranform_percentage_data(
                data_source_community,
                transformation = tranformation
            ) %>%
            tibble::column_to_rownames("sample_id")

        # drop empty cols and rows
        data_com <-
            data_com[
                rowSums(data_com) > 0,
                colSums(data_com) > 0
            ]

        switch(sel_method,
            "unconstrained" = {
                res <-
                    fit_ordination_dca(
                        data_source = data_com
                    )
            },
            "constrained" = {
                data_pred <-
                    data_source_predictors %>%
                    dplyr::filter(sample_id %in% rownames(data_com)) %>%
                    tibble::column_to_rownames("sample_id") %>%
                    dplyr::select(
                        dplyr::any_of(
                            c(
                                var_name_pred
                            )
                        )
                    )

                res <-
                    fit_ordination_dcca(
                        data_source_resp = data_com,
                        data_source_pred = data_pred,
                        sel_complexity = sel_complexity,
                        downweight = FALSE
                    )
            }
        )

        # get the range of first axis
        axis_1_range <-
            res %>%
            purrr::pluck("case_r") %>%
            purrr::pluck("axis_1") %>%
            range()

        # calculate total gradient length
        axis_1_grad_length <-
            (max(axis_1_range) - min(axis_1_range)) %>%
            abs()

        res$axis_1_grad_length <-
            axis_1_grad_length

        return(res)
    }
