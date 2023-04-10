#' @title Estimate taxonomic diversity using Hill numbers
#' @param data_matrix
#' Data matrix. Rows as levels and columns as taxa.
#' Row names should be the `sample_id`.
#' @param sample_size Numeric. Minimum sample size
#' @return
#' #' Data frame with diversity metric estimated for each level (sample):
#' \itemize{
#' \item `n0` - Hill' N0
#' \item `n1` - Hill' N1
#' \item `n2` - Hill' N2
#' \item `n1_minus_n2` - evenness ratios (N1-N2)
#' \item `n2_divided_by_n1` - evenness ratios (N2/N1)
#' \item `n1_divided_by_n0` - evenness ratios (N1/N0)
#' }
#' @description
#' Estimation of interpolation and extrapolation of individual-based.
#' This function estimates taxonomic diversity using Hill numbers.
#' It takes a data matrix as input, with rows representing samples and columns
#' representing taxa, and returns a dataframe with diversity indices for
#' each sample.
#' The diversity indices calculated are: Hill number 0, 1, and 2,
#' Hill number 1 minus Hill number 2, Hill number 2 divided by Hill number 1,
#' and Hill number 1 divided by Hill number 0
#' Hill numbers
#' @seealso [diversity_estimate()]
#' @author Vivian Felde, Ondrej Mottl
#' @export
diversity_estimate_taxonomic <-
  function(data_matrix,
           sample_size) {
    RUtilpol::check_class("data_matrix", c("data.frame", "matrix"))

    if (
      missing(sample_size)
    ) {
      sample_size <- min(apply(data_matrix, 1, sum))
    }

    RUtilpol::check_class("sample_size", "numeric")

    assertthat::assert_that(
      round(sample_size) == sample_size,
      msg = "'sample_size' has be integer"
    )

    # helper functions
    hill0 <-
      function(data, sample_size) {
        data_sub <- data[data > 0]
        data_sum <- sum(data_sub)
        if (
          sample_size <= data_sum
        ) {
          res <- sum(1 - exp(lchoose(data_sum - data_sub, sample_size) -
            lchoose(data_sum, sample_size)))
          return(res)
        } else {
          return(0)
        }
      }

    fk.hat <-
      function(data, sample_size) {
        data_sub <- data[data > 0]
        data_sum <- sum(data_sub)
        if (
          sample_size <= data_sum
        ) {
          sub <-
            function(k) {
              sum(exp(lchoose(data_sub, k) +
                lchoose(data_sum - data_sub, sample_size - k) -
                lchoose(data_sum, sample_size)))
            }
          res <- sapply(1:sample_size, sub)
          return(res)
        } else {
          return(0)
        }
      }

    hill1 <-
      function(data, sample_size) {
        data_sub <- data[data > 0]
        data_sum <- sum(data_sub)
        if (
          sample_size <= data_sum
        ) {
          k <- 1:sample_size
          res <-
            exp(-sum(k / sample_size *
              log(k / sample_size) *
              fk.hat(data_sub, sample_size)))
          return(res)
        } else {
          return(0)
        }
      }

    hill2 <-
      function(data, sample_size) {
        data_sub <- data[data > 0]
        data_sum <- sum(data_sub)
        if (
          sample_size <= data_sum
        ) {
          res <- 1 / (1 / sample_size +
            (1 - 1 / sample_size) *
              sum(data_sub * (data_sub - 1) / data_sum / (data_sum - 1)))
          return(res)
        } else {
          return(0)
        }
      }

    # estimate the diversity indeces
    est_n_0 <-
      sapply(
        sample_size,
        function(n) {
          apply(data_matrix,
            1,
            hill0,
            sample_size = n
          )
        }
      )

    est_n_1 <-
      sapply(
        sample_size,
        function(n) {
          apply(data_matrix,
            1,
            hill1,
            sample_size = n
          )
        }
      )

    est_n_2 <-
      sapply(
        sample_size,
        function(n) {
          apply(data_matrix,
            1,
            hill2,
            sample_size = n
          )
        }
      )

    # bind results
    hill_diversity <-
      cbind(
        est_n_0,
        est_n_1,
        est_n_2,
        est_n_1 - est_n_2,
        est_n_2 / est_n_1,
        est_n_1 / est_n_0
      ) %>%
      as.data.frame() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(sample_id = row.names(data_matrix)) %>%
      tibble::column_to_rownames("sample_id") %>%
      purrr::set_names(
        nm = c("n0", "n1", "n2", "n1_minus_n2", "n2_divided_by_n1", "n1_divided_by_n0")
      )

    return(hill_diversity)
  }
