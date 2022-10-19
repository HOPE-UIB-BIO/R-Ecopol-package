#' @title add age time bin 
#' @param data_source 
#' Data frame with age represented by `age_var_name` variable.
#' @param age_var_name
#' Character. Name of the column with age.
#' @param bin_var_name
#' Character. Name of the new colun with binned ages.
#' @param bin_size
#' Numeric. Size of the time bin.
#' @param sel_method
#' Character. Which direction should the values be binned.
#' \itemize{
#' \item `"backward"` - c(59, 105) -> c(0, 100)
#' \item `"forward"` - c(59, 105) -> c(100, 200)
#' }
util_add_age_bin <-
    function(data_source,
             age_var_name = "age",
             bin_var_name = "BIN",
             bin_size = 1000,
             sel_method = c("backward", "forward")) {

        util_check_class("data_source", "data.frame")

        util_check_class("age_var_name", "character")

        util_check_col_names("data_source", eval(age_var_name))

        util_check_class("bin_var_name", "character")

        util_check_class("bin_size", "numeric")

        util_check_class("sel_method", "character")

        util_check_vector_values("sel_method", c("backward", "forward"))

        sel_method <- match.arg(sel_method)

        if (
            sel_method == "backward"
        ) {
            data_source %>%
                dplyr::mutate(
                    !!bin_var_name := floor(get(age_var_name) / bin_size) * bin_size
                ) %>%
                return()
        } else if (
            sel_method == "forward"
        ) {
            data_source %>%
                dplyr::mutate(
                    !!bin_var_name := ceiling(get(age_var_name) / bin_size) * bin_size
                ) %>%
                return()
        }
    }
