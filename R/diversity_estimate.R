#' @title Estimate diversity for community data
#' @param data_source Data frame with rows as levels and columns as taxa. First
#' columns should be named `sample_id`.
#' @param sel_method Character. Selected method of diversify estimation
#' \itemize{
#' \item `"taxonomic"` - individual-based Hill numbers (diversity and evenness)
#' \item `"phylogenetic_diversity"` - Faith's phylogenetic diversity
#' \item `"phylogenetic_nri"` - Net Relatedness Index
#' \item `"phylogenetic_nti"` - Nearest Taxon Index
#' }
#' @param sample_size Numeric. Only for `"taxonomic"`. Minimum sample size
#' @param abundance_weighted Logical. Only for `"phylogenetic"`. It FALSE,
#' presence/absence data will be used.
#' @param rand Numeric. Number of randomisations
#' @param iterations Numeric. Only for `"phylogenetic"`. Number of iterations
#' to use for each randomization (for independent swap and trial null models).
#' @param phylogenetic_tree Only for `"phylogenetic"`. Phylogenetic backbone tree
#' constructed using `ape` package.
#' @description Estimation of diversity from community data.
#' @returns Data frame with diversity metric estimated for each level (sample).
#' Possible outputs depending on the `sel_method`:
#' \itemize{
#' \item `n0` -
#' \item `n1` -
#' \item `n2` -
#' \item `n1_minus_n2` -
#' \item `n2_divided_by_n1` -
#' \item `n1_divided_by_n0` -
#' \item `n_taxa` -
#' \item `faith_pd_diversity` -
#' \item `mean_pairwise_distances` -
#' \item `net_relatedness_index` -
#' \item `mean_nearest_taxon_distances` -
#' \item `nearest_taxon_index` -
#' \item `z_score` -
#' \item `p_value` -
#' }
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
