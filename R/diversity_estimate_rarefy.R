#' @title Estimation of interpolation and extrapolation of individual-based Hill number
#' @param data_matrix Data matrix
#' @param sample_size minimum sample size
#' @param set_margin Are levels as columns or rows? 1 = rows, 2 = columns
#' @return Matrix with Hills number N0, N1, and N2, evenness ratios, and error estimates
#' @description Estimation of interpolation and extrapolation of individual-based Hill number
diversity_estimate_rarefy <-
  function(data_matrix,
           sample_size,
           set_margin = 1){

    util_check_class("data_matrix", "matrix")

    util_check_class("set_margin", "numeric")

    if(missing(sample_size)){
      sample_size <- min(apply(data_matrix, set_margin, sum))
    }

    util_check_class("sample_size", "numeric")

    assertthat::assert_that(
      round(sample_size) == sample_size,
      msg = "'sample_size' has be integer")

    # helper functions
    hill0 <-
      function(data, sample_size){
        data_sub <- data[data > 0]
        data_sum <- sum(data_sub)
        if(sample_size <= data_sum){
          res <- sum(1-exp(lchoose(data_sum - data_sub, sample_size) -
                             lchoose(data_sum, sample_size)))
          return(res)
        } else {
          return(0)
        }
      }

    fk.hat <-
      function(data, sample_size){
        data_sub <- data[data > 0]
        data_sum <- sum(data_sub)
        if(sample_size <= data_sum){
          sub <-
            function(k) sum(exp(lchoose(data_sub, k) +
                                  lchoose(data_sum - data_sub, sample_size - k) -
                                  lchoose(data_sum, sample_size)))
          res <- sapply(1:sample_size, sub)
          return(res)
        } else {
          return(0)
        }
      }

    hill1 <-
      function(data, sample_size){
        data_sub <- data[data > 0]
        data_sum <- sum(data_sub)
        if(sample_size <= data_sum){
          k <- 1:sample_size
          res <-
            exp(-sum(k/sample_size *
                       log(k/ sample_size) *
                       fk.hat(data_sub, sample_size)))
          return(res)
        } else {
          return(0)
        }
      }

    hill2 <-
      function(data, sample_size){
        data_sub <- data[data > 0]
        data_sum <- sum(data_sub)
        if(sample_size <= data_sum){
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
      sapply(sample_size,
             function(n) apply(data_matrix,
                               set_margin,
                               hill0,
                               sample_size = n))

    est_n_1 <-
      sapply(sample_size,
             function(n) apply(data_matrix,
                               set_margin,
                               hill1,
                               sample_size = n))

    est_n_2 <-
      sapply(sample_size,
             function(n) apply(data_matrix,
                               set_margin,
                               hill2,
                               sample_size = n))


    # bind results
    hill_diversity <-
      cbind(est_n_0,
            est_n_1,
            est_n_2,
            est_n_1 - est_n_2,
            est_n_2 / est_n_1,
            est_n_1 / est_n_0) %>%
      tibble::as_tibble()

    names(hill_diversity) <-
      c("n0", "n1", "n2", "n1_minus_n2", "n2_divided_by_n1", "n1_divided_by_n0")

    return(hill_diversity)
  }
