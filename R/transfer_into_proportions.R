#' @title Transform pollen data into percentages
#' @param method variable to select result as either percentage (`percentages`)
#' or proportions (`percentages`).
#' @param data_source Data frame with pollen data. Each row represent one
#' level (sample) and each column represent one taxon. Table must contain
#' `sample_id` column with unique values.
#' @description Tranform pollen data into percentages (or proportions)
#' @export
transfer_into_proportions <-
  function(
    data_source,
    method = c("percentages", "proportions")) {

    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "sample_id")

    RUtilpol::check_class("method", "character")

    RUtilpol::check_vector_values("method", c("percentages", "proportions"))

    method <- match.arg(method)

    data_rownames <-
      data_source %>%
      tibble::column_to_rownames("sample_id")

    data_percentages <-
      data_rownames/rowSums(data_rownames) * switch(method,
                                                    "percentages" = 100,
                                                    "proportions" = 1)

    data_filter <-
      data_percentages[, colSums(data_percentages) > 0]

    data_filter %>%
      tibble::rownames_to_column("sample_id") %>%
      dplyr::relocate(sample_id) %>%
      tibble::as_tibble() %>%
      return()

  }
