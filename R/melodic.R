#' @title Mean Dissimilarity Components
#' @param samp community matrix; sites in lines, species in columns
#' @param dis dissimilarity matrix
#' @param type
#' \itemize{
#' \item `"both"` - for results with abundances weighted and non-weighted
#' \item `"abundance"` - for results with abundances weighted
#' \item `"presence"` - for results with abundances non-weighted
#' }
#' @description Function `melodic` obtained from https://doi.org/10.1007/s00442-016-3546-0
#' @author Carlos P. Carmona
melodic <-
  function(
    samp,
    dis,
    type = "both"){

    if
    (
      class(samp) != "matrix"
    ) {
      samp <-
        as.matrix(samp)
    }

    if
    (
      class(dis) != "matrix"
    ) {
      dis <-
        as.matrix(dis)
    }

    if
    (
      is.null(colnames(samp)) | is.null(colnames(dis))
    ) {
      stop("Both samp and dis must have colnames.\n")
    }

    N <- dim(samp)[1]

    melodic <- list()

    if
    (
      type == "both"
    ) {

      melodic$abundance <-
        list()

      melodic$abundance$mpd <-
        melodic$abundance$rao <-
        melodic$abundance$simpson <-
        numeric(N)

      melodic$presence <-
        list()

      melodic$presence$mpd <-
        melodic$presence$rao <-
        melodic$presence$simpson <-
        numeric(N)

    }
    else if
    (
      type == "abundance"
    ) {

      melodic$abundance <-
        list()

      melodic$abundance$mpd <-
        melodic$abundance$rao <-
        melodic$abundance$simpson <-
        numeric(N)
    }
    else if
    (
      type == "presence"
    ) {

      melodic$presence <-
        list()

      melodic$presence$mpd <-
        melodic$presence$rao <-
        melodic$presence$simpson <-
        numeric(N)
    }
    for (i in 1:N){

      sppInSample <-
        names(samp[i, samp[i, ] > 0])

      melodic$richness[i] <-
        rowSums(samp > 0)[i]

      if (length(sppInSample) > 1){

        sample.dis <-
          dis[sppInSample, sppInSample]

        abund.w <-
          numeric(
            length(sppInSample)
          )

        if
        (
          type == "both" | type == "abundance"
        ) {

          abund.w <-
            samp[i, sppInSample] / sum(samp[i, sppInSample])

          sample.weights <-
            outer(abund.w , abund.w)

          melodic$abundance$mpd[i] <-
            stats::weighted.mean(
              sample.dis[lower.tri(sample.dis)],
              sample.weights[lower.tri(sample.weights)]
            )

          melodic$abundance$rao[i] <-
            sum(sample.weights * sample.dis)

          melodic$abundance$simpson[i] <-
            sum(2 * sample.weights[lower.tri(sample.weights)])

        }
        if
        (
          type == "both" | type == "presence"
        ) {

          abund.nw <-
            rep(1, length(sppInSample)) / length(sppInSample)

          sample.weights <-
            outer(abund.nw, abund.nw)

          melodic$presence$mpd[i] <-
            stats::weighted.mean(
              sample.dis[lower.tri(sample.dis)],
              sample.weights[lower.tri(sample.weights)]
            )

          melodic$presence$rao[i] <-
            sum(sample.weights * sample.dis)

          melodic$presence$simpson[i] <-
            sum(2 * sample.weights[lower.tri(sample.weights)])
        }
      }	else {
        if
        (
          type=="both" | type=="abundance"
        )
        {

          melodic$abundance$mpd[i] <- NA

          melodic$abundance$rao[i] <-
            melodic$abundance$simpson[i] <- 0
        }
        if
        (
          type == "both" | type == "presence"
          ) {

          melodic$presence$mpd[i] <- NA

          melodic$presence$rao[i] <-
            melodic$presence$simpson[i] <-0
        }
      }
    }

    out <-
      melodic

    return(out)
  }
