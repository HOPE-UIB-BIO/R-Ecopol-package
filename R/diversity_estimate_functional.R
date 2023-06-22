#' @title Estimate functional diversity
#' @param data_source_community
#' Data matrix. Rows as levels and columns as taxa.
#' Row names should be the `sample_id`.
#' @param data_source_traits
#' Data frame. Row names be the same taxa as
#' in `data_source_community` and vice versa.
#' @param sel_method
#' Selected method for diversity estimation:
#' \itemize{
#' \item `"rao"` - Rao's quadratic diversity
#' \item `"mpd"` = Mean Pairwise Distance
#' \item `"simpson"` = Simpsons Diversity Index
#' }
#' @param abundance_weighted
#' Logical. A value indicating whether or not the calculation should be
#' abundance-weighted (TRUE) or presence/absence-based (FALSE).
#' @param rand
#' Numeric. An integer specifying the number of randomizations to perform to
#' obtain null distributions
#' @return
#' Data frame with diversity metric estimated for each level (sample).
#' Possible outputs depending on the `sel_method`:
#' \itemize{
#' \item `fd_rao` - Rao's quadratic diversity
#' \item `fd_mpd` - Mean Pairwise Distance
#' \item `fd_simpson` - Simpsons Diversity Index
#' \item `z_score` - Standarise Effect Size (SES), calculated as
#' (observed value - mean randomised value ) / sd of randomised value
#' }
#' @description
#' This function estimates functional diversity using various methods and
#' allows for randomization to obtain null distributions. This function is
#' a wrapper for the `melodic` adapted from
#' https://doi.org/10.1007/s00442-016-3546-0.
#' @author Triin Reitalu, Ondrej Mottl
#' @seealso [diversity_estimate()], [melodic()]
#' @export
diversity_estimate_functional <-
  function(data_source_community,
           data_source_traits,
           sel_method = c("rao", "mpd", "simpson"),
           abundance_weighted = TRUE,
           rand = 1000) {
    RUtilpol::check_class("data_source_community", c("data.frame", "matrix"))

    RUtilpol::check_class("data_source_traits", "data.frame")

    RUtilpol::check_class("abundance_weighted", "logical")

    sel_method <- match.arg(sel_method)

    RUtilpol::check_class("sel_method", "character")

    RUtilpol::check_vector_values(
      "sel_method",
      c("rao", "mpd", "simpson")
    )

    RUtilpol::check_class("rand", "numeric")

    assertthat::assert_that(
      rand == round(rand),
      msg = "'rand' must be a 'integer'"
    )

    sel_abund <-
      ifelse(abundance_weighted, "abundance", "presence")

    trait_dis <-
      vegan::vegdist(
        data_source_traits,
        method = "gower",
        na.rm = TRUE
      ) %>%
      as.matrix()

    # pre-allocate space
    rand_value <-
      matrix(
        nrow = nrow(data_source_community),
        ncol = rand
      )

    for (i in 1:rand) {
      # sample rows of trait matrix
      traits_rand <-
        data_source_traits[sample(1:nrow(data_source_traits)), ]

      # put the original names to the trait matrix
      rownames(traits_rand) <- rownames(data_source_traits)

      # calculate the trait distance from the randomized trait matrix
      trait_dis_rand <-
        vegan::vegdist(
          traits_rand,
          method = "gower",
          na.rm = TRUE
        ) %>%
        as.matrix()

      # calculate randomised value
      rand_value[, i] <-
        melodic(data_source_community, trait_dis_rand) %>%
        purrr::pluck(sel_abund) %>%
        purrr::pluck(sel_method)
    }

    result <-
      tibble::tibble(
        .rows = nrow(data_source_community)
      ) %>%
      dplyr::mutate(
        # observe value
        obs_value = melodic(data_source_community, trait_dis) %>%
          purrr::pluck(sel_abund) %>%
          purrr::pluck(sel_method),
        # mean value of randomised value
        rand_value_mean = rowMeans(rand_value),
        # standard deviations of randomized value
        rand_value_sd = apply(rand_value, 1, sd),
        # Standardise effect size
        z_score = (obs_value - rand_value_mean) / rand_value_sd,
        sample_id = rownames(data_source_community)
      ) %>%
      tibble::column_to_rownames("sample_id") %>%
      dplyr::select(obs_value, z_score) %>%
      rlang::set_names(
        nm = c(
          paste0("fd_", sel_method),
          "z_score"
        )
      )

    return(result)
  }
