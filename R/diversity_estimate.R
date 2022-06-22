#' @title Estimate diversity for community data
#' @param data_source
#' Data frame with rows as levels and columns as taxa. First
#' columns should be named `sample_id`.
#' @param sel_method
#' Character. Selected method of diversify estimation
#' \itemize{
#' \item `"taxonomic"` - individual-based Hill numbers of taxonomic diversity
#' (diversity and evenness)
#' \item `"phylogenetic_diversity"` - phylogenetic Faith's diversity
#' \item `"phylogenetic_nri"` - phylogenetic Net Relatedness Index
#' \item `"phylogenetic_nti"` - phylogenetic Nearest Taxon Index
#' \item `"functional_rao"` - functional Rao's quadratic diversity
#' \item `"functional_mpd"` - functional Mean Pairwise Distance
#' \item `"functional_simpson"` - functional Simpsons Diversity Index
#' }
#' @param round Logical. Should pollen values be rounded?
#' @param sample_size
#' Numeric. Only for `"taxonomic"`. Minimum sample size
#' @param abundance_weighted
#' Logical. Only for `"phylogenetic"` or `"functional"`.
#'  It FALSE, presence/absence data will be used for calculations.
#' @param rand
#' Numeric. Number of randomisations
#' @param iterations
#' Numeric. Only for `"phylogenetic"`. Number of iterations
#' to use for each randomization (for independent swap and trial null models).
#' @param phylogenetic_tree
#' Only for `"phylogenetic"`. Phylogenetic backbone tree constructed
#' using `ape` package.
#' @description
#' Estimation of diversity from community data. TODO
#' @returns
#' Data frame with diversity metric estimated for each level (sample).
#' Possible outputs depending on the `sel_method`:
#' \itemize{
#' \item `n0` - taxonomic Hill' N0
#' \item `n1` - taxonomic Hill' N1
#' \item `n2` - taxonomic Hill' N2
#' \item `n1_minus_n2` - taxonomic evenness ratios (N1-N2)
#' \item `n2_divided_by_n1` -  taxonomic evenness ratios (N2/N1)
#' \item `n1_divided_by_n0` - taxonomic evenness ratios (N1/N0)
#' \item `n_taxa` - Taxonomic richness
#' \item `pd_faith` - Faith's phylogenetic diversity
#' \item `pd_mpd` - phylogenetic Mean Pairwise Distance
#' \item `pd_nri` - phylogenetic Net Relatedness Index
#' \item `pd_mntd` - phylogenetic Mean Nearest Taxon Distance
#' \item `pd_nti` - phylogenetic Nearest Taxon Index
#' \item `fd_rao` - Rao's quadratic diversity
#' \item `fd_mpd` - Mean Pairwise Distance
#' \item `fd_simpson` - Simpsons Diversity Index
#' \item `z_score` - Standarise Effect Size (SES), calculated as
#' (observed value - mean randomised value ) / sd of randomised value
#' }
#' @export
diversity_estimate <-
  function(data_source,
           sel_method = c(
             "taxonomic",
             "phylogenetic_diversity",
             "phylogenetic_nri",
             "phylogenetic_nti",
             "functional_rao",
             "functional_mpd",
             "functional_simpson"
           ),
           round = TRUE,
           sample_size,
           rand = 999,
           iterations = 1000,
           abundance_weighted = TRUE,
           phylogenetic_tree = NULL) {
    util_check_class("data_source", "data.frame")

    util_check_col_names("data_source", "sample_id")

    sel_method <- match.arg(sel_method)

    util_check_class("sel_method", "character")

    util_check_vector_values(
      "sel_method",
      c(
        "taxonomic",
        "phylogenetic_diversity",
        "phylogenetic_nri",
        "phylogenetic_nti"
      )
    )

    util_check_class("rand", "numeric")

    assertthat::assert_that(
      rand == round(rand),
      msg = "'rand' must be a 'integer'"
    )

    if
    (
      round == TRUE
    ) {
      # remove sample_id, round down, and transform into matrix
      data_matrix <-
        data_source %>%
        tibble::column_to_rownames("sample_id") %>%
        dplyr::mutate_all(., .f = floor) %>%
        as.matrix() %>%
        round()
    } else {
      # remove sample_id, and transform into matrix
      data_matrix <-
        data_source %>%
        tibble::column_to_rownames("sample_id") %>%
        as.matrix()
    }

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
        msg = "'sample_size' has be integer"
      )

      div <-
        diversity_estimate_taxonomic(
          data_matrix = data_matrix,
          sample_size = sample_size
        )
    } else if
    (
      sel_method == "phylogenetic_diversity"
    ) {
      div <-
        diversity_estimate_phylogenetic(
          data_matrix = data_matrix,
          sel_method = "diversity",
          abundance_weighted = abundance_weighted,
          rand = rand,
          iterations = iterations,
          phylogenetic_tree = phylogenetic_tree
        )
    } else if
    (
      sel_method == "phylogenetic_nri"
    ) {
      div <-
        diversity_estimate_phylogenetic(
          data_matrix = data_matrix,
          sel_method = "nri",
          abundance_weighted = abundance_weighted,
          rand = rand,
          iterations = iterations,
          phylogenetic_tree = phylogenetic_tree
        )
    } else if
    (
      sel_method == "phylogenetic_nti"
    ) {
      div <-
        diversity_estimate_phylogenetic(
          data_matrix = data_matrix,
          sel_method = "nti",
          abundance_weighted = abundance_weighted,
          rand = rand,
          iterations = iterations,
          phylogenetic_tree = phylogenetic_tree
        )
    } else if
    (
      sel_method == "functional_rao"
    ) {
      div <-
        diversity_estimate_functional(
          data_source_community = data_matrix,
          data_source_traits = data_source_traits,
          abundance_weighted = abundance_weighted,
          sel_method = "rao",
          rand = rand
        )
    } else if
    (
      sel_method == "functional_mpd"
    ) {
      div <-
        diversity_estimate_functional(
          data_source_community = data_matrix,
          data_source_traits = data_source_traits,
          abundance_weighted = abundance_weighted,
          sel_method = "mpd",
          rand = rand
        )
    } else if
    (
      sel_method == "functional_simpson"
    ) {
      div <-
        diversity_estimate_functional(
          data_source_community = data_matrix,
          data_source_traits = data_source_traits,
          abundance_weighted = abundance_weighted,
          sel_method = "simpson",
          rand = rand
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
