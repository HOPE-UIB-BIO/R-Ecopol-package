#' @title Estimating phylogenetic diversity
#' @param data_matrix Data frame with rows as levels and columns as taxa
#' @param tree Phylogenetic backbone tree constructed using ape package
#' @param sel_method Selected method for diversity estimation.
#' \itemize{
#' \item `"diversity"` - Faith's phylogenetic diversity
#' \item `"NRI"` = Net Relatedness Index
#' \item `"NTI"` = Nearest Taxon Index
#' }
#' @param abundance_weighted Logical. It FALSE, presence absence data will
#'  be used.
#' @param rand Number of randomization
#' @param iterations Number of iterations to use for each randomization
#'  (for independent swap and trial null models)
#' @description Function for estimating one of three metrics of phylogenetic
#'  diversity
#' @return
#' Data frame with diversity metric estimated for each level (sample).
#' Possible outputs depending on the `sel_method`:
#' \itemize{
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
diversity_estimate_phylogenetic <-
  function(data_matrix, tree, dataset_id,
           sel_method = c("diversity", "NRI", "NTI"),
           abundance_weighted = TRUE,
           rand = 999,
           iterations = 1000) {

    util_check_class("data_matrix", "data.frame")

    util_check_class("tree", "phylo")

    util_check_class("abundance_weighted", "logical")

    sel_method <- match.arg(sel_method)

    util_check_class("sel_method", "character")

    util_check_vector_values("sel_method", c("diversity", "NRI", "NTI"))

    util_check_class("rand", "numeric")

    assertthat::assert_that(
      rand ==  round(rand),
      msg = "'rand' must be a 'integer'")

    util_check_class("iterations", "numeric")

    assertthat::assert_that(
      iterations ==  round(iterations),
      msg = "'iterations' must be a 'integer'")

    assertthat::assert_that(
      all(colnames(dat) %in% tree$tip.label),
      msg = "All taxa names from 'data_matrix' has to be present in 'tree'")

    if
    (
      ncol(dat) <= 2
    ) {
      cat("Dataset does not have enough taxons to estimate phylogenetic diversity", "\n")

      return(NA)
    }

    # Make a vector of families from the tree that are missing from the data_matrix data
    drop_list <-
      tree$tip.label[!tree$tip.label %in% colnames(dat)]

    # Remove all the families that do not occur in our sample
    pruned_tree <- ape::drop.tip(tree, drop_list)

    # Re-order the taxa
    data_ordered <- dat[, c(pruned_tree$tip.label)]

    # Create cophenetic distance from the pruned tree.
    phy_dist <- stats::cophenetic(pruned_tree)

    if
    (
      sel_method == "diversity"
    ) {
      # Calculate Faith's pylogenetic diversity

      cat("Estimating Faith's pylogenetic diversity", "\n")

      res <-
        picante::ses.pd(
          samp = data_ordered,
          tree = pruned_tree,
          null.model = "taxa.labels",
          rand = rand,
          iterations = iterations,
          include.root = TRUE) %>%
        tibble::rownames_to_column("sample_id") %>%
        tibble::as_tibble() %>%
        dplyr::rename(
          n_taxa = ntaxa,
          faith_pd_diversity = pd.obs,
          z_score = pd.obs.z,
          p_value = pd.obs.p) %>%
        dplyr::select(sample_id, n_taxa, faith_pd_diversity, z_score, p_value)
    } else if
    (
      sel_method == "NRI"
    ) {
      # Calculate NRI

      cat("Estimating Net Relatedness Index (NRI)", "\n")

      res <-
        picante::ses.mpd(
          samp = data_ordered,
          dis = phy_dist,
          null.model = "taxa.labels",
          abundance.weighted = abundance_weighted,
          rand = rand,
          iterations = iterations) %>%
        tibble::rownames_to_column("sample_id") %>%
        tibble::as_tibble() %>%
        dplyr::rename(
          n_taxa = ntaxa,
          mean_pairwise_distances = mpd.obs,
          p_value = mpd.obs.p) %>%
        dplyr::mutate(net_relatedness_index = mpd.obs.z*(-1)) %>%
        dplyr::select(sample_id, n_taxa, mean_pairwise_distances, net_relatedness_index, p_value)
    } else if
    (
      sel_method == "NTI"
    ) {
      # Calculate NRI

      cat("Estimating Nearest Taxon Index (NTI)", "\n")

      res <-
        picante::ses.mntd(
          samp = data_ordered,
          dis = phy_dist,
          null.model = "taxa.labels",
          abundance.weighted = abundance_weighted,
          rand = rand,
          iterations = iterations) %>%
        tibble::rownames_to_column("sample_id") %>%
        tibble::as_tibble() %>%
        dplyr::rename(
          n_taxa = ntaxa,
          mean_nearest_taxon_distances = mntd.obs,
          p_value = mntd.obs.p) %>%
        dplyr::mutate(nearest_taxon_index = mntd.obs.z*(-1)) %>%
        dplyr::select(sample_id, n_taxa, mean_nearest_taxon_distances, nearest_taxon_index, p_value)
    }

    return(res)

  }
