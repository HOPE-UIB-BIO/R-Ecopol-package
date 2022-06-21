#' @title Estimate of functional diversity
#' @param data_source_community Data matrix. Rows as levels and columns as data_source_community. Row names
#' should be the `sample_id`.
#' @param data_source_traits Data matrix. Row names and col names must be the same
#' as data_source_community in `data_source_community`
#' @param rand Number of randomization
#' @return TODO
#' @description TODO
#' @export
diversity_estimate_functional <-
  function(data_source_community,
           data_source_traits,
           sel_method = c("rao", "mpd", "simpson"),
           rand = 1000) {

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
        purrr::pluck("abundance") %>%
        purrr::pluck(sel_method)

    }

    result <-
      tibble::tibble(
        .rows = nrow(data_source_community)
      ) %>%
      dplyr::mutate(
        # observe value
        obs_value =  melodic(data_source_community, trait_dis) %>%
          purrr::pluck("abundance") %>%
          purrr::pluck(sel_method),
        # mean value of randomised value
        rand_value_mean = rowMeans(rand_value),
        # standard deviations of randomized value
        rand_value_sd = apply(rand_value, 1, sd),
        # Standardise effect size
        SES = (obs_value - rand_value_mean) / rand_value_sd,
        sample_id = rownames(data_source_community)
      ) %>%
      tibble::column_to_rownames("sample_id")

    return(result)
  }
