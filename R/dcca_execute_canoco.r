
#' @title Execute CANOCO
#' @description
#' # Performs single execution of command-line CANOCO program
#' with specified response and predictor data, using DCCA after
#' square-root transforming. Returns analysis results as
#' a list (described below). On failures calls the stop
#' function, so it might be wise to enclose the call in a try
#' construct to catch such cases
#' @param data_resp
#' data frame with response data (pollen counts or percentages)
#' @param data_pred
#' data frame with required predictors (all will be used)
#' @param default_name
#' how to name CON, OUT and SOL files (e.g. xx.con, xx.out, xx.sol)
#' Default value "xx"
#' The `default_name`` is also used for produced data files:
#' \itemize{
#' \item xx-pred.dta
#' \item xx-resp.dta
#' }
#' @param downweight
#' logical - whether to downweight rare species
#' Default value FALSE
#' @return
#' \itemize{
#' \item `eig` - numeric vector - eigenvalues for the first four axes
#' \item `tot_inertia` - total variation in (transformed) response data
#' \item `turn` - numeric vector with turnover values
#' \item `case_e` - numeric matrix with CaseE scores for (up to) first 3 axes
#' \item `case_r` - numeric matrix with CaseR scores for first 4 axes
#' }
#' @author Petr Smilauer
#' @keywords internal
dcca_execute_canoco <-
  function(data_resp,
           data_pred,
           default_name = "xx",
           downweight = FALSE) {
    if (
      (.Platform["OS.type"] == "windows") == FALSE
    ) {
      stop(
        "the `dcca_execute_canoco` funtion works only on Windows machine"
      )
    }

    # (1) Create data files -----

    # absolute path to the folder where canoco.exe is located
    #   and where also the intermediate results will be stored
    #   (so it should be writable)
    sel_path <-
      system.file("exec", package = "REcopol")

    path_length <-
      nchar(sel_path)

    # append slash to path, if not present
    if (
      substr(sel_path, path_length, path_length) != "/"
    ) {
      sel_path <-
        paste0(
          sel_path, "/"
        )
    }

    # test the presence of canoco.exe
    if (
      file.exists(
        paste0(
          sel_path,
          "canoco.exe"
        )
      ) == FALSE
    ) {
      stop(
        "Cannot find 'canoco.exe' in the REcopol package folder"
      )
    }

    # fully qualified name of response data file
    resp_file_path <-
      paste0(
        sel_path, default_name, "-resp.dta"
      )

    # fully qualified name of  predictor data file
    pred_file_path <-
      paste0(
        sel_path, default_name, "-pred.dta"
      )

    # create response data file
    if (
      dcca_make_can_file(
        sel_path = resp_file_path,
        sel_data = data_resp
      ) == FALSE
    ) {
      util_clean_files(sel_path)
      stop(
        "Cannot export response data into ",
        resp_file_path,
        call. = FALSE
      )
    }

    # create predictor data file
    if (
      dcca_make_can_file(
        sel_path = pred_file_path,
        sel_data = data_pred
      ) == FALSE
    ) {
      util_clean_files(sel_path)
      stop(
        "Cannot export predictor data into ",
        pred_file_path,
        call. = FALSE
      )
    }


    # (2) Create CON file -----
    con_file_path <-
      paste(
        sel_path, default_name, ".con",
        sep = ""
      )

    if (
      dcca_make_con_file(
        con_file_path = con_file_path,
        resp_file_path = resp_file_path,
        pred_file_path = pred_file_path,
        default_name = default_name,
        downweight = downweight
      ) == FALSE
    ) {
      util_clean_files(sel_path)
      stop(
        "Cannot produce CON file into ",
        con_file_path,
        call. = FALSE
      )
    }


    # (3) Execute CANOCO command line -----
    current_work_dir <-
      getwd()

    setwd(sel_path)

    system2(
      "canoco.exe",
      args = con_file_path
    )

    # (4) Parse OUT file -----
    out_file_path <-
      paste(
        default_name, ".out",
        sep = ""
      )

    if (
      file.exists(out_file_path) == FALSE
    ) {
      util_clean_files(sel_path)
      stop(
        "Cannot find produced file ",
        out_file_path,
        call. = FALSE
      )
    }

    out_file <-
      file(
        out_file_path,
        open = "r"
      )

    # first determine the number of active sample:
    sel_line <-
      util_read_lines_until(
        connection_file = out_file,
        sel_text = " No. of active  samples:"
      )

    n_rows <-
      as.numeric(
        (strsplit(sel_line, ":", fixed = T)[[1]])[2]
      )

    n_rows_tot <-
      dim(data_resp)[1]

    if (
      (n_rows < 1) | (n_rows > n_rows_tot)
    ) {
      util_clean_files(sel_path)
      stop(
        "Wrong count of active samples ",
        n_rows,
        " in output",
        call. = FALSE
      )
    }

    sel_line <-
      util_read_lines_until(
        connection_file = out_file,
        sel_text = " Eigenvalues  "
      )

    if (
      nchar(sel_line) < 20
    ) {
      util_clean_files(sel_path)
      stop(
        "Cannot parse produced file ",
        out_file_path,
        call. = FALSE
      )
    }

    # Retrieve eigenvalues and total inertia
    eigs <-
      as.numeric(
        util_get_line_by_tabs(sel_line, 2, 6)
      )

    tot_inertia <-
      eigs[5]

    eigs <-
      eigs[1:4]

    # Retrieve and parse turnover size
    sel_line <-
      util_read_n_lines(
        connection_file = out_file,
        num_lines = 1
      )

    turns <-
      as.numeric(
        util_get_line_by_tabs(
          sel_line = sel_line,
          id_from = 2,
          id_to = 5
        )
      )

    # close OUT file
    close(out_file)


    # (5) Parse SOL file -----
    sol_file_path <-
      paste(default_name, ".sol", sep = "")

    if (
      file.exists(sol_file_path) == FALSE
    ) {
      util_clean_files(sel_path)
      stop(
        "Cannot find produced file ",
        sol_file_path,
        call. = FALSE
      )
    }

    sol_file <-
      file(sol_file_path, open = "r")

    sel_line <-
      util_read_lines_until(
        connection_file = sol_file,
        sel_text = " Samp: Sample scores"
      )

    if (
      nchar(sel_line) < 20
    ) {
      util_clean_files(sel_path)
      stop(
        "Cannot parse produced file [1] ",
        sol_file_path,
        call. = FALSE
      )
    }

    # read through another five lines
    sel_line <-
      util_read_n_lines(
        connection_file = sol_file,
        num_lines = 5
      )

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
      sel_line <-
        util_read_n_lines(
          connection_file = sol_file,
          num_lines = 1
        )

      # separate its fields
      str_entries <-
        strsplit(sel_line, "\t", fixed = TRUE)[[1]]

      # identify case index
      i_case <-
        as.numeric(str_entries[1])

      if (
        (i_case < 1) | (i_case > n_rows_tot) | (length(str_entries) < 8)
      ) {
        util_clean_files(sel_path)
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
        dimnames(data_resp)[[1]],
        c("Axis 1", "Axis 2", "Axis 3", "Axis 4")
      )

    # Now read the second set of case scores
    sel_line <-
      util_read_lines_until(
        connection_file = sol_file,
        sel_text = " SamE: Sample scores"
      )

    if (
      nchar(sel_line) < 20
    ) {
      util_clean_files(sel_path)
      stop(
        "Cannot parse [2] produced file ",
        sol_file_path,
        call. = FALSE
      )
    }

    sel_line <-
      util_read_n_lines(
        connection_file = sol_file,
        num_lines = 5
      )

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
      sel_line <-
        util_read_n_lines(
          connection_file = sol_file,
          num_lines = 1
        )

      # separate its fields
      str_entries <-
        strsplit(sel_line, "\t", fixed = T)[[1]]

      # identify case index
      j_case <-
        as.numeric(str_entries[1])

      if (
        (j_case < 1) | (j_case > n_rows_tot) | (length(str_entries) < 7)
      ) {
        util_clean_files(sel_path)
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
        dimnames(data_resp)[[1]],
        c("Axis 1", "Axis 2", "Axis 3", "Axis 4")
      )

    # close SOL file
    close(sol_file)

    setwd(current_work_dir) # undo change of working directory of R

    util_clean_files(sel_path)

    # (6) Return results -----
    return(
      list(
        eig = eigs,
        tot_inertia = tot_inertia,
        turn = turns,
        case_e = case_e,
        case_r = case_r
      )
    )
  }
