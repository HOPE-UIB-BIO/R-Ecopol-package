#' @title  Re-number cluster groups
#' @param clusgr Vector with integers
#' @description Utility function to re-number vector of integers to simple
#' sequence increased by one
renumber_groups <-
  function(clusgr){

    aa <- 1

    renum <-
      rep(1, length(clusgr))

    for(i in 2:length(clusgr)){

      if(clusgr[i] != clusgr[i - 1]) aa <- aa + 1

      renum[i] <- aa
    }
    return(renum)
  }
