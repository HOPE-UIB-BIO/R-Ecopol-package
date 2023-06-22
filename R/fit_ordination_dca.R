#' @title Fit DCA
#' @description
#' Fits a DCA using [vegan::decorana()].
#' @param data_source
#' data frame with response data (pollen counts or percentages). Sample ID as
#' rownames.
#' @seealso [vegan::decorana()]
#' @return
#' \itemize{
#' \item `ordination` - ordination object of class "decorana"
#' \item `case_r` - numeric matrix with CaseR scores for first 4 axes
#' }
fit_ordination_dca <-
    function(data_source) {
        RUtilpol::check_class("data_source", "data.frame")

        res_ord <-
            vegan::decorana(
                veg = data_source
            )

        case_r_formated <-
            res_ord %>%
            vegan::scores(.,
                display = "sites",
                scaling = "sites",
                origin = FALSE
            ) %>%
            as.data.frame() %>%
            rlang::set_names(
                nm = c(
                    paste0("axis_", 1:4)
                )
            ) %>%
            # there is a rounding error which result in values smaller than 0
            #   There is a simple replacement with 0
            dplyr::mutate(
                dplyr::across(
                    .cols = dplyr::everything(),
                    .fns = ~ ifelse(.x < 0, 0, .x)
                )
            ) %>%
            tibble::rownames_to_column("sample_id") %>%
            tibble::as_tibble()

        return(
            list(
                ordination = res_ord,
                case_r = case_r_formated
            )
        )
    }
