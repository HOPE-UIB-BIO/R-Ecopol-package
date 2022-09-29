#' @title Cretae CON file
#' @description
#' Creates CANOCO 4.5x project (.CON) file for a specific
#' type of analysis (DCCA with detrending by segments)
#' @param con_file_path
#' qualified name of CON file to create
#' @param resp_file_path
#' name of (CANOCO-format) file with response data
#' @param pred_file_path
#' name of (CANOCO-format) file with explanatory data
#' @param default_name
#' temporary file name default_name (for .OUT and .SOL files)
#' (default "xx")
#' @param downweight
#' logical value: down-weighting of rare species
#' (default `FALSE`)
#' @param n_segments
#' number of segments to use for detrending
#' (default 26)
#' @param n_rescale
#' number of times to apply non-linear rescaling
#' (default 4)
#' @param rescale_threshold
#' threshold for non-linear rescaling
#' (default 0.0)
#' @return
#' TRUE on success, FALSE otherwise
#' @author Petr Smilauer
dcca_make_con_file <-
  function(con_file_path,
           resp_file_path,
           pred_file_path,
           default_name = "xx",
           downweight = FALSE,
           n_segments = 26,
           n_rescale = 4,
           rescale_threshold = 0.0) {
    if (
      (file.exists(resp_file_path) == FALSE) |
        (file.exists(pred_file_path) == FALSE) |
        (file.create(con_file_path) == FALSE)
    ) {
      return(FALSE)
    }

    f_lines <-
      vector("character")

    f_lines[1] <-
      "     2"

    f_lines[2] <-
      "     1 =  long dialogue?"

    f_lines[3] <-
      "  0 = changing maximum sizes?"

    f_lines[4] <-
      paste("", resp_file_path) # insert space at start

    f_lines[5] <-
      " S" # no covariates

    f_lines[6] <-
      paste("", pred_file_path) # insert space at start

    f_lines[7] <-
      paste(
        " ", default_name, ".out", # OUT file, e.g. " xx.out"
        sep = ""
      )

    f_lines[8] <-
      paste(
        " ", default_name, ".sol", # solution file, e.g. " xx.sol"
        sep = ""
      )

    f_lines[9] <-
      " 8   = analysis number (DCCA)"

    f_lines[10] <-
      " 1   = detrending by segments"

    f_lines[11] <-
      paste(
        " ", n_segments, "                  = number of segments",
        sep = ""
      )

    f_lines[12] <-
      paste(
        "  ", n_rescale, " = rescaling of axes",
        sep = ""
      )

    f_lines[13] <-
      paste(
        "   ",
        formatC(rescale_threshold, digits = 2, format = "f"),
        "= rescaling threshold"
      )

    f_lines[14] <-
      "  2 = no. of axes in spec-env biplot"

    f_lines[15] <-
      "  0 = spec and sample diagnostics"

    f_lines[16] <-
      "     0 = sample number to be omitted"

    f_lines[17] <-
      "  0 = select/delete of environmental variables"

    f_lines[18] <-
      "    0     0  = product of environmental variables"

    f_lines[19] <-
      "   0 = no transformation of species data"

    f_lines[20] <-
      "     1.00000 = weight for species  ( noweight=1)"

    f_lines[21] <-
      "     1.00000 = weight for  samples ( noweight=1)"

    if (
      downweight == TRUE
    ) {
      f_lines[22] <-
        "  1 = weighting of species?"
    } else {
      f_lines[22] <-
        "  0 = weighting of species?"
    }

    f_lines[23] <-
      "  0 = output of correlations?"

    f_lines[24] <-
      "  0  2  0  0  0  0  0  2 = output just CaseR and CaseE"

    # open CON file for writing
    con_file <-
      file(con_file_path, open = "wt")

    # write formed lines to it
    writeLines(f_lines, con_file)

    # close it
    close(con_file)

    return(TRUE)
  }
