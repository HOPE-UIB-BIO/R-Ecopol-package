#' @title Add partition of data
#' @param data_source_var Data frame with the variables to partition
#' @param data_source_levels Data frame with ages of levels
#' @param age_var Name (in quotes) of variable which define the age of samples
#' @description Add partition for each column for chronologically ordered data using regression trees
#' @export
add_data_partition <-
  function(data_source_var,
           data_source_levels,
           age_var = "age") {

    util_check_class("data_source_var", "data.frame")

    util_check_class("data_source_levels", "data.frame")

    util_check_class("age_var", "character")

    util_check_col_names("data_source_var", "sample_id")

    util_check_col_names("data_source_levels", c("sample_id", age_var))

    data_work <-
      dplyr::inner_join(
        data_source_levels %>%
          dplyr::select(
            dplyr::all_of(
              c("sample_id", eval(age_var))
            )),
        data_source_var %>%
          dplyr::select(sample_id, where(is.double)),
        by = "sample_id")

    list_partitions <-
      data_work %>%
      tidyr::pivot_longer(
        cols = -c(sample_id, eval(age_var)),
        names_to = "type",
        values_to = "estimate") %>%
      base::split(.$type) %>%
      purrr::map(
        .f = ~ regression_partition(
          data_source = .x,
          var = "estimate",
          age_var = age_var))

    final_tibble <-
      dplyr::bind_cols(
        data_source_var,
        list_partitions %>%
          purrr::map_dfc("rpart_groups") %>%
          dplyr::rename_all(
            .funs = ~ paste0(.x,"_part")))

    return(final_tibble)

  }
