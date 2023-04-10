#' @title Transform pollen data using selected transformation
#' @param data_source Data frame with pollen data. Each row represent one
#' level (sample) and each column represent one taxon. Table must contain
#' `sample_id` column with unique values.
#' @param transformation Selection of data transformation options. `hellinger` =
#' Hellinger, `chisq` = Chisq, `none` = without transformation. Both Hellinger
#' and Chisq are recommend for proportional/percentage data
#' @description Transform pollen data using one of the selected transformation
#' functions
#' @export 
tranform_percentage_data <-
  function(
    data_source,
    transformation = c("chisq", "hellinger", "none")) {

    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "sample_id")

    RUtilpol::check_class("transformation", "character")

    RUtilpol::check_vector_values("transformation", c("hellinger", "chisq", "none"))

    transformation <- match.arg(transformation)

    data_rownames <-
      data_source %>%
      tibble::column_to_rownames("sample_id")


    if (transformation == "hellinger") {
      data_trans <-
        vegan::decostand(data_rownames,
                         method = "hellinger")
    } else if (transformation == "chisq") {
      data_trans <-
        vegan::decostand(data_rownames,
                         method = "chi.square")
    } else if (transformation == "none") {
      data_trans <- data_rownames
    }

    data_trans %>%
      tibble::rownames_to_column("sample_id") %>%
      dplyr::relocate(sample_id) %>%
      tibble::as_tibble() %>%
      return()

  }
