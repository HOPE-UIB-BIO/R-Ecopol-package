#' @title Mean Dissimilarity Components
#' @param data_source_community
#' community matrix; sites in lines, species in columns
#' @param data_source_dissimilarity dissimilarity matrix
#' @description Function `melodic` adapted from
#' https://doi.org/10.1007/s00442-016-3546-0
#' @author Carlos P. Carmona, Ondrej Mottl
#' @references
#' de Bello, F., Carmona, C.P., Leps, J. et al.
#' Functional diversity through the mean trait dissimilarity:
#' resolving shortcomings with existing paradigms and algorithms.
#' Oecologia 180, 933-940 (2016).
#' https://doi.org/10.1007/s00442-016-3546-0
#' @keywords internal
melodic <-
  function(data_source_community,
           data_source_dissimilarity) {
    # check for
    RUtilpol::check_class(
      "data_source_community", "matrix"
    )


    RUtilpol::check_class(
      "data_source_dissimilarity",
      c("matrix", "dist")
    )

    if (
      !any(class(data_source_dissimilarity) == "matrix")
    ) {
      data_source_dissimilarity <-
        as.matrix(data_source_dissimilarity)
    }

    assertthat::assert_that(
      is.null(colnames(data_source_community)) == FALSE &&
        is.null(colnames(data_source_dissimilarity)) == FALSE,
      msg = paste0(
        "Both data_source_community and data_source_dissimilarity",
        "must have colnames.\n"
      )
    )

    assertthat::assert_that(
      ncol(data_source_community) == ncol(data_source_dissimilarity) &&
        ncol(data_source_community) == nrow(data_source_dissimilarity),
      msg = paste(
        "'data_source_community' and 'data_source_dissimilarity' must",
        "have the same number of taxa"
      )
    )

    assertthat::assert_that(
      all(sort(colnames(data_source_dissimilarity)) == sort(colnames(data_source_community))) &
        all(sort(rownames(data_source_dissimilarity)) == sort(colnames(data_source_community))),
      msg = paste(
        "'data_source_community' and 'data_source_dissimilarity' must",
        "have the same taxa names"
      )
    )

    n_row <- nrow(data_source_community)

    # pre-allocate space
    melodic <- list()

    melodic$abundance <-
      melodic$presence <-
      list()

    melodic$richness <-
      melodic$abundance$mpd <-
      melodic$abundance$rao <-
      melodic$abundance$simpson <-
      melodic$presence$mpd <-
      melodic$presence$rao <-
      melodic$presence$simpson <-
      numeric(n_row)

    for (i in 1:n_row) {
      spp_in_sample <-
        names(data_source_community[i, data_source_community[i, ] > 0])

      melodic$richness[i] <-
        length(spp_in_sample)

      if (
        length(spp_in_sample) <= 1
      ) {
        melodic$abundance$mpd[i] <- NA

        melodic$abundance$rao[i] <-
          melodic$abundance$simpson[i] <- 0

        melodic$presence$mpd[i] <- NA

        melodic$presence$rao[i] <-
          melodic$presence$simpson[i] <- 0

        next
      }

      sample_dis <-
        data_source_dissimilarity[
          rownames(data_source_dissimilarity) %in% spp_in_sample,
          base::colnames(data_source_dissimilarity) %in% spp_in_sample
        ]

      abund_w <-
        numeric(
          length(spp_in_sample)
        )

      abund_w <-
        data_source_community[i, spp_in_sample] /
          sum(data_source_community[i, spp_in_sample])

      sample_weights <-
        outer(abund_w, abund_w)

      melodic$abundance$mpd[i] <-
        stats::weighted.mean(
          sample_dis[lower.tri(sample_dis)],
          sample_weights[lower.tri(sample_weights)]
        )

      melodic$abundance$rao[i] <-
        sum(sample_weights * sample_dis)

      melodic$abundance$simpson[i] <-
        sum(2 * sample_weights[lower.tri(sample_weights)])

      abund_nw <-
        rep(1, length(spp_in_sample)) / length(spp_in_sample)

      sample_weights_nw <-
        outer(abund_nw, abund_nw)

      melodic$presence$mpd[i] <-
        stats::weighted.mean(
          sample_dis[lower.tri(sample_dis)],
          sample_weights_nw[lower.tri(sample_weights_nw)]
        )

      melodic$presence$rao[i] <-
        sum(sample_weights_nw * sample_dis)

      melodic$presence$simpson[i] <-
        sum(2 * sample_weights_nw[lower.tri(sample_weights_nw)])
    }

    out <-
      melodic

    return(out)
  }
