#' @title Estimation of interpolation and extrapolation of individual-based Hill number
#' @param data_source Data frame with rows as levels and columns as taxa
#' @param sel_method Selected method of diversify estimation c("randomised", "rarefy")
#' @param sample_size minimum sample size
#' @param rand number of randomisations
#' @description Estimation of interpolation and extrapolation of individual-based Hill number
#' @returns Tibble including phylogenetic diversity metric for each level.
#' @export
diversity_estimate <-
  function(data_source,
           sel_method = c("rarefy", "randomised"),
           sample_size,
           rand = 999) {


    util_check_class("data_source", "data.frame")

    # remove sample_id, round down, and tranform into matrix
    data_matrix <-
      data_source %>%
      dplyr::select(!dplyr::any_of("sample_id")) %>%
      dplyr::mutate_all(., .f = floor) %>%
      as.matrix()

    sel_method <- match.arg(sel_method)

    util_check_class("sel_method", "character")

    util_check_vector_values("sel_method", c("rarefy", "randomised"))

    if(missing(sample_size)){
      sample_size <-
        apply(data_matrix, 1, sum) %>%
        floor() %>%
        min()
    }

    util_check_class("sample_size", "numeric")

    assertthat::assert_that(
      round(sample_size) == sample_size,
      msg = "'sample_size' has be integer")


    if(sel_method == "randomised") {

      util_check_class("rand", "numeric")

      assertthat::assert_that(
        rand ==  round(rand),
        msg = "'rand' must be a 'integer'")


      div <-
        diversity_estimate_randomise(
          data_matrix = round(data_matrix),
          sample_size = sample_size,
          set_margin = 1,
          rand = rand)

    } else if (sel_method == "rarefy"){
      div <-
        diversity_estimate_rarefy(
          data_matrix = round(data_matrix),
          sample_size = sample_size,
          set_margin = 1)
    }

    res <-
      div %>%
      as.data.frame() %>%
      tibble::as_tibble()


    # add sample_id if present in the original data
    if("sample_id" %in% names(data_source)){
      res <-
        res %>%
        dplyr::mutate(
          sample_id = data_source$sample_id) %>%
        dplyr::relocate(sample_id)
    }
    return(res)
  }
