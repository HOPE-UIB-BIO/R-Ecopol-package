
#' @title Execute CANOCO
#' @description
#' # Performs single execution of command-line CANOCO program
#' with specified response and predictor data, using DCCA after
#' square-root transforming. Returns analysis results as
#' a list (described below). On failures calls the stop
#' function, so it might be wise to enclose the call in a try
#' construct to catch such cases
#' @param path
#' absolute path to the folder where .EXE is located
#' and where also the intermediate results will be stored
#' (so it should be writable)
#' @param resp
#' data frame with response data (pollen counts or percentages)
#' @param pred
#' data frame with required predictors (all will be used)
#' @param base
#' how to name CON, OUT and SOL files (e.g. xx.con, xx.out, xx.sol)
#' Default value "xx"
#' The 'base' is also used for produced data files:
#' /itemize{
#' /item xx-pred.dta
#' /item xx-resp.dta
#' }
#' @param downweight
#' logical - whether to downweight rare species
#' Default value FALSE
#' @return
#' /itemize{
#' /item `eig` - numeric vector - eigenvalues for the first four axes
#' /item  `tot_inertia` - total variation in (transformed) response data
#' /item `turn` - numeric vector with turnover values
#' /item `case_r` - numeric matrix with CaseE scores for (up to) first 3 axes
#' /item `case_r` - numeric matrix with CaseR scores for first 4 axes
#' }
#' @author Petr Smilauer
dcca_execute_canoco <-
  function(path,
           resp,
           pred,
           base = "xx",
           downweight = FALSE) {

    # (1) Create data files -----
    path.len <-
      nchar(path)

    # append slash to path, if not present
    if (
      substr(path, path.len, path.len) != "/"
    ) {
      path <-
        paste(
          path, "/",
          sep = ""
        )
    }

    # fully qualified name of response data file
    resp_fname <-
      paste(
        path, base, "-resp.dta",
        sep = ""
      )

    # create response data file
    if (
      dcca_export_can_file(resp_fname, resp) == FALSE
    ) {
      stop(
        "Cannot export response data into ",
        resp_fname,
        call. = FALSE
      )
    }

    # fully qualified name of  predictor data file
    pred_fname <-
      paste(
        path, base, "-pred.dta",
        sep = ""
      )

    # create predictor data file
    if (
      dcca_export_can_file(pred_fname, pred) == FALSE
    ) {
      file.remove(resp_fname)
      stop(
        "Cannot export predictor data into ",
        pred_fname,
        call. = FALSE
      )
    }


    # (2) Create CON file -----
    con_fname <-
      paste(
        path, base, ".con",
        sep = ""
      )

    if (
      dcca_export_con_file(
        con_fname,
        resp_fname,
        pred_fname,
        base,
        downweight
      ) == FALSE
    ) {
      file.remove(resp_fname)
      file.remove(pred_fname)
      stop(
        "Cannot produce CON file into ",
        con_fname,
        call. = FALSE
      )
    }


    # (3) Execute CANOCO command line -----
    WD <-
      getwd()

    setwd(path)

    system2(
      "Canoco.exe",
      args = con_fname
    )

    # (4) Parse OUT file -----
    out_fname <-
      paste(
        base, ".out",
        sep = ""
      )

    if (
      file.exists(out_fname) == FALSE
    ) {
      file.remove(resp_fname)
      file.remove(pred_fname)
      file.remove(con_fname)
      stop(
        "Cannot find produced file ",
        out_fname,
        call. = FALSE
      )
    }

    out_file <-
      file(
        out_fname,
        open = "r"
      )

    # first determine the number of active sample:
    a_line <-
      util_read_to_line(
        out_file,
        " No. of active  samples:"
      )

    n_rows <-
      as.numeric(
        (strsplit(a_line, ":", fixed = T)[[1]])[2]
      )

    n_rows_tot <-
      dim(resp)[1]

    if (
      (n_rows < 1) | (n_rows > n_rows_tot)
    ) {
      file.remove(resp_fname)
      file.remove(pred_fname)
      file.remove(con_fname)
      stop(
        "Wrong count of active samples ",
        n_rows,
        " in output",
        call. = FALSE
      )
    }

    a_line <-
      util_read_to_line(
        out_file,
        " Eigenvalues  "
      )

    if (
      nchar(a_line) < 20
    ) {
      file.remove(resp_fname, pred_fname, con_fname)

      # file.remove( out_fname);
      stop(
        "Cannot parse produced file ",
        out_fname,
        call. = FALSE
      )
    }

    # Retrieve eigenvalues and total inertia
    eigs <-
      as.numeric(
        util_parse_line_by_tabs(a_line, 2, 6)
      )

    tot_inertia <-
      eigs[5]

    eigs <-
      eigs[1:4]

    # Retrieve and parse turnover size
    a_line <-
      util_read_n_lines(out_file, 1)

    turns <-
      as.numeric(
        util_parse_line_by_tabs(a_line, 2, 5)
      )

    # close OUT file
    close(out_file)


    # (5) Parse SOL file -----
    sol_fname <-
      paste(base, ".sol", sep = "")

    if (
      file.exists(sol_fname) == FALSE
    ) {
      file.remove(resp_fname, pred_fname, con_fname, out_fname)
      stop(
        "Cannot find produced file ",
        sol_fname,
        call. = FALSE
      )
    }

    sol_file <-
      file(sol_fname, open = "r")

    a_line <-
      util_read_to_line(
        sol_file,
        " Samp: Sample scores"
      )

    if (
      nchar(a_line) < 20
    ) {
      file.remove(resp_fname, pred_fname, con_fname, out_fname)
      stop(
        "Cannot parse produced file [1] ",
        sol_fname,
        call. = FALSE
      )
    }

    # read through another five lines
    a_line <-
      util_read_n_lines(sol_file, 5)

    # prepare storage full of NA values
    case_r <-
      matrix(
        as.vector(
          rep(NA, n_rows_tot * 4),
          mode = "numeric"
        ),
        ncol = 4
      )

    # read single line
    for (i in 1:n_rows) {
      a_line <-
        util_read_n_lines(sol_file, 1)

      # separate its fields
      str_entries <-
        strsplit(a_line, "\t", fixed = T)[[1]]

      # identify case index
      i_case <-
        as.numeric(str_entries[1])

      if (
        (i_case < 1) | (i_case > n_rows_tot) | (length(str_entries) < 8)
      ) {
        file.remove(resp_fname, pred_fname, con_fname, out_fname)
        stop(
          "Error parsing CaseR scores for entry ",
          i,
          call. = FALSE
        )
      }

      # store four CaseR scores
      case_r[i_case, ] <-
        as.numeric(str_entries[3:6])
    }

    # Assign row and column names
    dimnames(case_r) <-
      list(
        dimnames(resp)[[1]],
        c("Axis 1", "Axis 2", "Axis 3", "Axis 4")
      )

    # Now read the second set of case scores
    a_line <-
      util_read_to_line(sol_file, " SamE: Sample scores")

    if (
      nchar(a_line) < 20
    ) {
      file.remove(resp_fname, pred_fname, con_fname, out_fname)
      stop(
        "Cannot parse [2] produced file ",
        sol_fname,
        call. = FALSE
      )
    }

    a_line <-
      util_read_n_lines(sol_file, 5)

    # prepare storage full of NA values
    case_e <-
      matrix(
        as.vector(
          rep(NA, n_rows_tot * 4),
          mode = "numeric"
        ),
        ncol = 4
      )

    for (j in 1:n_rows) {
      # read one line
      a_line <-
        util_read_n_lines(sol_file, 1)

      # separate its fields
      str_entries <-
        strsplit(a_line, "\t", fixed = T)[[1]]

      # identify case index
      j_case <-
        as.numeric(str_entries[1])

      if (
        (j_case < 1) | (j_case > n_rows_tot) | (length(str_entries) < 7)
      ) {
        file.remove(resp_fname, pred_fname, con_fname, out_fname)
        stop(
          "Error parsing CaseE scores for entry ",
          j,
          call. = FALSE
        )
      }

      # store four CaseE scores
      case_e[j_case, ] <-
        as.numeric(str_entries[3:6])
    }

    # Assing row and column names
    dimnames(case_e) <-
      list(
        dimnames(resp)[[1]],
        c("Axis 1", "Axis 2", "Axis 3", "Axis 4")
      )

    # close SOL file
    close(sol_file)

    setwd(WD) # undo change of working directory of R


    # (6) Return results -----
    list(
      eig = eigs,
      tot_inertia = tot_inertia,
      turn = turns,
      case_e = case_e,
      case_r = case_r
    ) %>%
      return()
  }
