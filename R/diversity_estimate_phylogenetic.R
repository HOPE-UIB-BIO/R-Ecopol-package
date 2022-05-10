#' @title Estimating phylogenetic diversity
#' @param data_source Data frame with rows as levels and columns as taxa
#' @param tree Phylogenetic diversity tree constructed using ape package
#' @param dataset_id Optional. unique dataset identification as character
#' @param method Selected method for diversity estimation.
#'  "diversity" = Faith's phylogenetic diversity
#'  "NRI" = Net Relatedness Index
#'  "NTI" = Nearest Taxon Index
#' @param abundance_weighted Logical. It FALSE, presence absence data will
#'  be used.
#' @param rand Number of randomization
#' @param iterations Number of iterations to use for each randomization
#'  (for independent swap and trial null models)
#' @description Function for estimating one of three metrics of phylogenetic
#'  diversity
#' @return Tibble with Hill numbers N0, N1, N2, the evenness ratios N2/N1, N2/N0, and the
#' modified versions of N2/N1 and N2/N0
#' @export
diversity_estimate_phylogenetic <-
  function(data_source, tree, dataset_id,
           method = c("diversity", "NRI", "NTI"),
           abundance_weighted = TRUE,
           rand = 999,
           iterations = 1000) {

    util_check_class("data_source", "data.frame")

    util_check_col_names("data_source", "sample_id")

    util_check_class("tree", "phylo")

    util_check_class("abundance_weighted", "logical")

    method <- match.arg(method)

    util_check_class("method", "character")

    util_check_vector_values("method", c("diversity", "NRI", "NTI"))

    util_check_class("rand", "numeric")

    assertthat::assert_that(
      rand ==  round(rand),
      msg = "'rand' must be a 'integer'")

    util_check_class("iterations", "numeric")

    assertthat::assert_that(
      iterations ==  round(iterations),
      msg = "'iterations' must be a 'integer'")

    if(!missing(dataset_id)){

      util_check_class("dataset_id", "character")

      cat(
        paste("Processing dataset", dataset_id), "\n")

    }

    dat <-
      data_source %>%
      as.data.frame() %>%
      tibble::column_to_rownames("sample_id")

    assertthat::assert_that(
      all(colnames(dat) %in% tree$tip.label),
      msg = "All taxa names from 'data_source' has to be present in 'tree'")

    if(ncol(dat) <= 2){
      cat("Dataset does not have enough taxons to estimate diversity", "\n")

      return(NA)
    }

    # Make a vector of families from the tree that are missing from the data_source data
    drop_list <-
      tree$tip.label[!tree$tip.label %in% colnames(dat)]

    # Remove all the families that do not occur in our sample
    pruned_tree <- ape::drop.tip(tree, drop_list)

    # Re-order the taxa
    data_ordered <- dat[, c(pruned_tree$tip.label)]

    # Create cophenetic distance from the pruned tree.
    phy_dist <- stats::cophenetic(pruned_tree)

    if(method == "diversity"){
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
          faith_pd_diverity = pd.obs,
          z_score = pd.obs.z,
          p_value = pd.obs.p) %>%
        dplyr::select(sample_id, n_taxa, faith_pd_diverity, z_score, p_value)
    } else if(method == "NRI"){
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
    } else if(method == "NTI"){
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
