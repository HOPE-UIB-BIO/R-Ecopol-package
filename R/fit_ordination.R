#' @title Fit ordination
#' @param data_source_community
#' Data.frame with community data. Each row is sample. First columns is
#' `sample_id`, each other is taxa.
#' @param data_source_predictors
#' Data.frame with predictors. First columns is `sample_id`. Other columns
#' can be predictors and/or conditional variables.
#' @param var_name_pred
#' Character. Vector with the names of predicotr variables.
#' @param var_name_cond
#' Character. Optional name of conditional variable
#' @param sel_method
#' \itemize{
#' \item `"cca"` - Correspondence Analysis
#' \item `"rda"` - Redundancy Analysis
#' \item `"dca"` - Detrended Correspondence Analysis
#' }
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
#' @description
#' This is a wrapper function to perform standard data preparation
#' and either unconstrained (DCA) or (partialy-)consatrined ordination (CCA/RDA)
#' with fossil pollen data. By default constrained ordination is constrained by
#' `"age"`.
#' @returns Ordniation from `vegan` package
#' @export
fit_ordination <-
    function(data_source_community,
             data_source_predictors = NULL,
             var_name_pred = c("age"),
             var_name_cond = NULL,
             sel_method = c("cca", "rda", "dca"),
             transform_to_percentage = FALSE,
             tranformation = c("none", "chisq", "hellinger")) {
        util_check_class("data_source_community", "data.frame")

        util_check_col_names("data_source_community", "sample_id")

        util_check_class("data_source_predictors", c("data.frame", "NULL"))

        if (
            is.null(var_name_cond) == FALSE
        ) {
            util_check_class("var_name_cond", "character")

            assertthat::assert_that(
                length(var_name_cond) == 1,
                msg = paste(
                    "Curently, the funcation can only handle single",
                    "conditional variable"
                )
            )
        }

        util_check_vector_values(
            "sel_method",
            c("rda", "cca", "dca")
        )

        sel_method <- match.arg(sel_method)

        util_check_class("sel_method", "character")

        if (
            sel_method != "dca"
        ) {
            util_check_col_names("data_source_predictors", "sample_id")

            util_check_class("var_name_pred", "character")

            util_check_col_names("data_source_predictors", var_name_pred)
        }

        util_check_class("transform_to_percentage", "logical")

        tranformation <- match.arg(tranformation)

        util_check_class("tranformation", "character")

        util_check_vector_values(
            "tranformation",
            c("none", "chisq", "hellinger")
        )

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

        community_sample_id <-
            data_source_community %>%
            purrr::pluck("sample_id")

        if (
            sel_method != "dca"
        ) {
            pred_sample_id <-
                data_source_predictors %>%
                purrr::pluck("sample_id")

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

        if (
            sel_method == "dca"
        ) {
            res_ord <-
                vegan::decorana(
                    veg = data_com
                )
            return(res_ord)
        }

        data_pred <-
            data_source_predictors %>%
            dplyr::filter(sample_id %in% rownames(data_com)) %>%
            tibble::column_to_rownames("sample_id") %>%
            dplyr::select(
                dplyr::any_of(
                    c(
                        var_name_pred,
                        var_name_cond
                    )
                )
            )

        # make the formula
        formula_raw <-
            paste0(
                "data_com ~ ",
                paste(
                    c(var_name_pred),
                    collapse = " + "
                )
            )

        if (
            is.null(var_name_cond) == FALSE
        ) {
            formula_work <-
                paste0(
                    formula_raw,
                    " + Condition(", var_name_cond, ")"
                ) %>%
                as.formula()
        } else {
            formula_work <-
                as.formula(formula_raw)
        }

        if (
            sel_method == "rda"
        ) {
            res_ord <-
                vegan::rda(
                    formula = formula_work,
                    data = data_pred,
                    na.action = na.omit
                )
        } else if (
            sel_method == "cca"
        ) {
            res_ord <-
                vegan::cca(
                    formula = formula_work,
                    data = data_pred,
                    na.action = na.omit
                )
        }

        return(res_ord)
    }
