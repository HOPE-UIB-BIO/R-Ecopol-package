#' @title Multivariate regression partitioning
#' @param data_source_counts Data frame with pollen data. Each row represent one
#' level (sample) and each column represent one taxon. Table must contain
#' `sample_id` column with unique values corresponding to `data_source_levels`
#' @param data_source_levels Data frame with level ages, with each row represent
#'  one level (sample). Table must contain two columns: `sample_id` with unique
#'  values corresponding to `data_source_levels`, and `age` with ages of each
#'  level
#' @param rand The number of times to randomly split the data to determine the 
#' optimal tree.
#' @inheritParams tranform_percentage_data
#' @description This function performs multivariate regression partitioning
#' using the mvpart R package.
#' It takes in a data frame of count data, a data frame of sample level
#' information, and various optional arguments, and returns a list of outputs
#' including the multivariate regression tree object, the change points, the
#' partition assignments, and the number of groups.
#' @return A list containing the following items:
#' \itemize{
#' \item{`mrt_result`}{The multivariate regression tree object.}
#' \item{`change_points`}{The change points in the regression tree.}
#' \item{`partitions`}{The partition assignments for each sample.}
#' \item{`mrt_groups`}{The number of groups in the partitioning.}
#' \item{`mrt_groups_per_sample`}{The number of groups per sample.}
#' }
#' @export
mv_regression_partition <-
  function(data_source_counts,
           data_source_levels,
           rand = 1000,
           transformation = c("chisq", "hellinger", "none")) {
    RUtilpol::check_class("data_source_counts", "data.frame")

    RUtilpol::check_col_names("data_source_counts", "sample_id")

    RUtilpol::check_class("data_source_levels", "data.frame")

    RUtilpol::check_col_names("data_source_levels", c("sample_id", "age"))

    RUtilpol::check_class("rand", "numeric")

    RUtilpol::check_class("transformation", "character")

    RUtilpol::check_vector_values("transformation", c("hellinger", "chisq", "none"))

    transformation <- match.arg(transformation)

    data_percentage <-
      transfer_into_proportions(
        data_source_counts,
        method = "percentages"
      )

    data_trans <-
      tranform_percentage_data(
        data_percentage,
        transformation = transformation
      ) %>%
      tibble::column_to_rownames("sample_id")

    age <-
      data_source_levels$age

    mvpart_result <-
      mvpart::mvpart(data.matrix(data_trans) ~ age,
        xv = "1se",
        xvmult = rand,
        plot.add = FALSE,
        data = data_trans
      )

    # wrapper to prevent any message
    capture.output(
      change_points_age <-
        as.data.frame(summary(mvpart_result)$splits) %>%
        purrr::pluck("index"),
      file = "NUL"
    )

    partitions <-
      tibble::tibble(
        sample_id = data_source_levels$sample_id,
        partition = util_renumber_groups(mvpart_result$where)
      )

    # number of partitions
    mrt_groups <- length(unique(partitions$partition))

    results <-
      list(
        mrt_result = mvpart_result,
        change_points = change_points_age,
        partitions = partitions,
        mrt_groups = mrt_groups,
        mrt_groups_per_sample = mrt_groups / nrow(data_trans)
      )

    return(results)
  }
