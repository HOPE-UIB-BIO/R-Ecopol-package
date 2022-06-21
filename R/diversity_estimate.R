#' @title Estimation of interpolation and extrapolation of individual-based Hill number
#' @param data_source Data frame with rows as levels and columns as taxa. First
#' row should be named `sample_id`.
#' @param sel_method Selected method of diversify estimation c("taxonomic_randomised", "taxonomic_rarefy")
#' @param sample_size minimum sample size
#' @param rand number of randomisations
#' @description Estimation of interpolation and extrapolation of individual-based Hill number
#' @returns Tibble including phylogenetic diversity metric for each level.
#' @export
diversity_estimate <-
  function(data_source,
           sel_method = c("taxonomic",
                          "phylogenetic_diversity",
                          "phylogenetic_nri",
                          "phylogenetic_nti"),
           sample_size,
           rand = 999,
           iterations = 1000,
           abundance_weighted = TRUE,
           phylogenetic_tree = NULL) {

    util_check_class("data_source", "data.frame")

    util_check_col_names("data_source", "sample_id")

    # remove sample_id, round down, and transform into matrix
    data_matrix <-
      data_source %>%
      tibble::column_to_rownames("sample_id") %>%
      dplyr::mutate_all(., .f = floor) %>%
      as.matrix() %>%
      round()

    sel_method <- match.arg(sel_method)

    util_check_class("sel_method", "character")

    util_check_vector_values(
      "sel_method",
      c("taxonomic",
        "phylogenetic_diversity",
        "phylogenetic_nri",
        "phylogenetic_nti")
    )

    util_check_class("rand", "numeric")

    assertthat::assert_that(
      rand ==  round(rand),
      msg = "'rand' must be a 'integer'")

    if
    (
      ncol(data_matrix) < 2
    ) {
      cat("Dataset does not have enough taxons to estimate diversity", "\n")

      return(NA)
    }

    if
    (
      sel_method == "taxonomic"
    ) {

      if
      (
        missing(sample_size)
      ) {
        sample_size <-
          apply(data_matrix, 1, sum) %>%
          floor() %>%
          min()
      }

      util_check_class("sample_size", "numeric")

      assertthat::assert_that(
        round(sample_size) == sample_size,
        msg = "'sample_size' has be integer")

      div <-
        diversity_estimate_taxonomic(
          data_matrix = data_matrix,
          sample_size = sample_size
        )

    } else if
    (
      sel_method == "phylogenetic_diversity"
    ) {

      diversity_estimate_phylogenetic(
        data_source = data_matrix,
        method = "diversity",
        abundance_weighted = abundance_weighted,
        rand = rand,
        iterations = iterations,
        phylogenetic_tree = phylogenetic_tree
      )

    } else if
    (
      sel_method == "phylogenetic_nri"
    ) {

      diversity_estimate_phylogenetic(
        data_source = data_matrix,
        method = "NRI",
        abundance_weighted = abundance_weighted,
        rand = rand,
        iterations = iterations,
        phylogenetic_tree = phylogenetic_tree
      )

    } else if
    (
      sel_method == "phylogenetic_nti"
    ) {

      diversity_estimate_phylogenetic(
        data_source = data_matrix,
        method = "NTI",
        abundance_weighted = abundance_weighted,
        rand = rand,
        iterations = iterations,
        phylogenetic_tree = phylogenetic_tree
      )

    }

    res <-
      div %>%
      tibble::rownames_to_column("sample_id") %>%
      dplyr::relocate(sample_id) %>%
      as.data.frame() %>%
      tibble::as_tibble()

    return(res)
  }
