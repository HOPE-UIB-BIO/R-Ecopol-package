#' @title Estimating phylogenetic diversity
#' @param data_matrix
#' Data matrix. Rows as levels and columns as data.
#' Row names should be the `sample_id`.
#' @param phylogenetic_tree
#' Phylogenetic backbone phylogenetic_tree constructed using `ape` package
#' @param sel_method
#' Selected method for diversity estimation.
#' \itemize{
#' \item `"diversity"` - Faith's phylogenetic diversity
#' \item `"nri"` = Net Relatedness Index
#' \item `"nti"` = Nearest Taxon Index
#' }
#' @param abundance_weighted
#' Logical. If FALSE, presence-absence data will be used.
#' @param rand
#' Numeric. Number of randomization
#' @param iterations
#' Number of iterations to use for each randomization
#'  (for independent swap and trial null models)
#' @description
#' Function for estimating one of three metrics of phylogenetic diversity. It is
#' a wrapper for various functions from {picante} package.
#' @return
#' Data frame with diversity metric estimated for each level (sample).
#' Possible outputs depending on the `sel_method`:
#' \itemize{
#' \item `n_taxa` - Taxonomic richness
#' \item `pd_faith` - Faith's phylogenetic diversity
#' \item `pd_mpd` - Mean Pairwise Distance
#' \item `pd_nri` - Net Relatedness Index
#' \item `pd_mntd` - Mean Nearest Taxon Distance
#' \item `pd_nti` - Nearest Taxon Index
#' \item `z_score` - Standarise Effect Size (SES), calculated as
#' (observed value - mean randomised value ) / sd of randomised value
#' }
#' @seealso [picante::ses.pd()], [picante::ses.mpd()], [picante::ses.mntd()],
#' [diversity_estimate()]
#' @author Ondrej Mottl, Kuber Bhatta
#' @export
diversity_estimate_phylogenetic <-
  function(data_matrix,
           phylogenetic_tree,
           sel_method = c("diversity", "nri", "nti"),
           abundance_weighted = TRUE,
           rand = 999,
           iterations = 1000) {
    RUtilpol::check_class("data_matrix", c("data.frame", "matrix"))

    RUtilpol::check_class("phylogenetic_tree", "phylo")

    RUtilpol::check_class("abundance_weighted", "logical")

    sel_method <- match.arg(sel_method)

    RUtilpol::check_class("sel_method", "character")

    RUtilpol::check_vector_values("sel_method", c("diversity", "nri", "nt"))

    RUtilpol::check_class("rand", "numeric")

    assertthat::assert_that(
      rand == round(rand),
      msg = "'rand' must be a 'integer'"
    )

    RUtilpol::check_class("iterations", "numeric")

    assertthat::assert_that(
      iterations == round(iterations),
      msg = "'iterations' must be a 'integer'"
    )

    assertthat::assert_that(
      all(colnames(data_matrix) %in% phylogenetic_tree$tip.label),
      msg = paste(
        "All taxa names from 'data_matrix' has to be present in",
        "'phylogenetic_tree'", "\n"
      )
    )

    if (
      ncol(data_matrix) <= 2
    ) {
      cat(
        paste(
          "Dataset does not have enough taxons to",
          "estimate phylogenetic diversity", "\n"
        )
      )

      return(NA)
    }

    # Make a vector of families from the phylogenetic_tree
    # that are missing from the data_matrix data
    drop_list <-
      phylogenetic_tree$tip.label[
        !phylogenetic_tree$tip.label %in% colnames(data_matrix)
      ]

    # Remove all the families that do not occur in our sample
    pruned_tree <- ape::drop.tip(phylogenetic_tree, drop_list)

    # Re-order the taxa
    data_ordered <- data_matrix[, c(pruned_tree$tip.label)]

    # Create cophenetic distance from the pruned phylogenetic_tree.
    phy_dist <- stats::cophenetic(pruned_tree)

    if (
      sel_method == "diversity"
    ) {
      # Calculate Faith's pylogenetic diversity

      cat("Estimating Faith's pylogenetic diversity", "\n")

      res <-
        picante::ses.pd(
          samp = data_ordered,
          tree = pruned_tree,
          null.model = "taxa.labels",
          runs = rand,
          iterations = iterations,
          include.root = TRUE
        ) %>%
        dplyr::rename(
          n_taxa = ntaxa,
          pd_faith = pd.obs,
          z_score = pd.obs.z,
        ) %>%
        dplyr::select(n_taxa, pd_faith, z_score)
    } else if (
      sel_method == "nri"
    ) {
      # Calculate nri

      cat("Estimating Net Relatedness Index (nri)", "\n")

      res <-
        picante::ses.mpd(
          samp = data_ordered,
          dis = phy_dist,
          null.model = "taxa.labels",
          abundance.weighted = abundance_weighted,
          runs = rand,
          iterations = iterations
        ) %>%
        dplyr::rename(
          n_taxa = ntaxa,
          pd_mpd = mpd.obs,
        ) %>%
        dplyr::mutate(pd_nri = mpd.obs.z * (-1)) %>%
        dplyr::select(n_taxa, pd_mpd, pd_nri)
    } else if (
      sel_method == "nti"
    ) {
      # Calculate nri

      cat("Estimating Nearest Taxon Index (nti)", "\n")

      res <-
        picante::ses.mntd(
          samp = data_ordered,
          dis = phy_dist,
          null.model = "taxa.labels",
          abundance.weighted = abundance_weighted,
          runs = rand,
          iterations = iterations
        ) %>%
        dplyr::rename(
          n_taxa = ntaxa,
          pd_mntd = mntd.obs,
        ) %>%
        dplyr::mutate(
          pd_nti = mntd.obs.z * (-1)
        ) %>%
        dplyr::select(n_taxa, pd_mntd, pd_nti)
    }

    return(res)
  }
