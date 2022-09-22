#' @title Select DCCA complexity
#' @description
#' This is a driver function that finds a specific DCCA model
#' depending on the comparison of first constrained and first
#' unconstrained eigenvalue
#' The single numerical predictor (depth or age) is tried first
#' as linear, then as poly(x,2), and finally as poly(x,3)
#' until L(1) > L(unc1)
#' You need to specify first three arguments to perform the analysis
#' The other two arguments are optional and adjust behaviour
#' @param path
#' is absolute path to the folder where CANOCO.EXE is located
#' and where also the intermediate results will be stored
#' (so it should be writable)
#' @param resp
#' data frame with response data (pollen counts or percentages)
#' @param pred_var
#' numeric vector with the predictor values (depth or age)
#' Its length must be identical with the count of rows in resp
#' data frame
#' @param base
#' how to name the CON, OUT and SOL files (e.g. xx.con, xx.out,xx.sol)
#' Also used for produced data files: xx-pred.dta, xx-resp.dta
#' @param downweight
#' logical, whether to downweight rare species or not
#' @return
#' /itemize{
#' /item `eig` - numeric vector - eigenvalues for the first four axes
#' /item  `tot_inertia` - total variation in (transformed) response data
#' /item `turn` - numeric vector with turnover values
#' /item `case_r` - numeric matrix with CaseE scores for (up to) first 3 axes
#' /item `case_r` - numeric matrix with CaseR scores for first 4 axes
#' /item `degree` - indicates how the pred_var was used:
#' 1 - as a linear predictor
#' 2 or 3 - as a corresponding polynomial
#' }
#' @author Petr Smilauer
dcca_select_can <-
  function(path,
           resp,
           pred_var,
           base = "xx",
           downweight = FALSE) {

    # Create data frame with a linear form of predictor
    pred_df <-
      data.frame(x = pred_var)

    dimnames(pred_df) <-
      list(
        dimnames(resp)[[1]],
        c("Predictr")
      )

    # Execute CANOCO
    res_1 <-
      dcca_execute_canoco(
        path,
        resp,
        pred_df,
        base,
        downweight
      )

    if (
      length(res_1$eig) < 4
    ) {
      stop("Error executing DCCA with a linear predictor effect")
    }

    if (
      res_1$eig[1] > res_1$eig[2] # appropriate result
    ) {
      res_1$degree <- 1
      return(res_1)
    }

    # Failed, so create 2nd-order polynomial
    #  Make a copy of predictor ...
    pred_df[, 2] <-
      pred_df[, 1]

    # ... and replace both columns with an orthogonal polynomial
    pred_df[, 1:2] <-
      poly(pred_df[, 1], 2)

    names(pred_df) <-
      c("Poly.1", "Poly.2")

    # ... re-execute DCCA
    res_1 <-
      dcca_execute_canoco(
        path,
        resp,
        pred_df,
        base,
        downweight
      )

    if (
      length(res_1$eig) < 4
    ) {
      stop("Error executing DCCA with a poly(x,2) predictor effect")
    }

    if (
      res_1$eig[1] > res_1$eig[3] # success
    ) {
      res_1$degree <- 2
      return(res_1)
    }

    # No help, so create 3rd-order polynomial
    pred_df[, 3] <-
      pred_var

    pred_df[, 1:3] <-
      poly(pred_var, 3)

    names(pred_df) <-
      c("Poly.1", "Poly.2", "Poly.3")

    # ... re-execute DCCA
    res_1 <-
      dcca_execute_canoco(
        path,
        resp,
        pred_df,
        base,
        downweight
      )

    if (
      length(res_1$eig) < 4
    ) {
      stop("Error executing DCCA with a poly(x,3) predictor effect")
    }

    if (
      res_1$eig[1] < res_1$eig[4]
    ) {
      warning(
        "Failed to get eigenvalue(Ax1) greater than the 1st unconstrained eigenvalue"
      )
    }

    res_1$degree <- 3

    return(res_1)
  }
