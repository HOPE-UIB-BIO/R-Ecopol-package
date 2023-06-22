#' @title Add partition of data
#' @param data_source_var A dataframe with samples as rows and variables as
#' columns. This dataframe must contain a "sample_id" column.
#' @param data_source_levels A dataframe with one row per sample, where the
#' sample_id column must match the data_source_var dataframe. This dataframe
#' must contain an "age" column.
#' @param age_var A character string indicating the name of the age column in
#' the data_source_levels dataframe. The default value is "age".
#' @description Add partition for each column for chronologically ordered
#' data using regression trees (`mvpart::rpart()`).
#' @return A dataframe with the same columns as data_source_var plus a column
#' for each partition, named as variable name plus "_part".
#' @seealso [regression_partition()], [mvpart::rpart()]
#' @export
add_data_partition <-
  function(data_source_var,
           data_source_levels,
           age_var = "age") {
    RUtilpol::check_class("data_source_var", "data.frame")

    RUtilpol::check_class("data_source_levels", "data.frame")

    RUtilpol::check_class("age_var", "character")

    RUtilpol::check_col_names("data_source_var", "sample_id")

    RUtilpol::check_col_names("data_source_levels", c("sample_id", age_var))

    # Join data_source_levels and data_source_var by the "sample_id" column
    data_work <-
      dplyr::inner_join(
        data_source_levels %>%
          dplyr::select(
            # Select the sample_id and age columns
            dplyr::all_of(
              c("sample_id", eval(age_var))
            )
          ),
        data_source_var %>%
          # Select only the columns that are double
          dplyr::select(sample_id, where(is.double)),
        by = "sample_id"
      )

    # Pivot data to long format
    list_partitions <-
      data_work %>%
      tidyr::pivot_longer(
        cols = -c(sample_id, eval(age_var)),
        names_to = "type",
        values_to = "estimate"
      ) %>%
      # Split data by type column
      base::split(.$type) %>%
      purrr::map(
        .f = ~ regression_partition(
          data_source = .x,
          var = "estimate",
          age_var = age_var
        )
      )

    # Bind the partitions to the original dataframe
    final_tibble <-
      dplyr::bind_cols(
        data_source_var,
        # Select only the "rpart_groups" column from each partition
        list_partitions %>%
          purrr::map_dfc("rpart_groups") %>%
          # Rename the columns to include the variable name and "_part"
          dplyr::rename_all(
            .funs = ~ paste0(.x, "_part")
          )
      )
    return(final_tibble)
  }
