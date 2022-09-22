#' @title Export CAN file
#' @description
#' Saves a data frame into a file in Canoco free format
#' path is the fully qualified name of the file to create
#' data is the data frame
#' @return
#' FALSE on failure, TRUE otherwise
#' @author Petr Smilauer
dcca_export_can_file <-
  function(path,
           data) {
    n_rows <-
      nrow(data)

    n_cols <-
      ncol(data)

    # Try to create an empty file first
    if (
      !file.create(path)
    ) {
      return(FALSE)
    }

    # f_lines will store all output lines
    f_lines <-
      vector("character")

    f_lines[1] <-
      "Free format file created from a data frame"

    f_lines[2] <-
      "FREE"

    f_lines[3] <-
      paste(n_cols, n_rows) # space separated

    id_x_line <- 4 # present line index ...

    for (i in 1:n_rows) {
      a_line <- ""

      for (j in 1:n_cols) {
        a_line <-
          paste(a_line, data[i, j])

        if (
          (j %% 10) == 0
        ) {

          # store current line state
          f_lines[id_x_line] <-
            a_line

          # reset to empty
          a_line <- ""

          # move to the next line index
          id_x_line <-
            id_x_line + 1
        }
      }

      # if we have something in the output buffer ...
      if (
        nchar(a_line) > 0
      ) {

        # store current line state
        f_lines[id_x_line] <-
          a_line

        # move to the next line index
        id_x_line <-
          id_x_line + 1
      }
    }

    # After data, store the names of variables

    a_line <- ""

    col_names <-
      dimnames(data)[[2]]

    for (i in 1:n_cols) {

      # left-aligned, padded with space
      a_line <-
        paste( # no space among the labels
          a_line,
          formatC(
            substring(col_names[i], 1, 8), # up to first 8 chars
            format = "s",
            width = 8,
            flag = "-"
          ),
          sep = ""
        )

      if (
        (i %% 10) == 0
      ) {

        # store current line
        f_lines[id_x_line] <-
          a_line

        # reset line to empty
        a_line <- ""

        # move to the next line index
        id_x_line <-
          id_x_line + 1
      }
    }


    if (
      nchar(a_line) > 0 # something left in otput buffer
    ) {

      # store current line
      f_lines[id_x_line] <-
        a_line

      # set buffer to empty
      a_line <- ""

      # progress to next line index
      id_x_line <-
        id_x_line + 1
    }

    row_names <-
      dimnames(data)[[1]]

    for (i in 1:n_rows) {

      # see above for parameters
      a_line <-
        paste(
          a_line,
          formatC(
            substring(row_names[i], 1, 8),
            width = 8,
            format = "s",
            flag = "-"
          ),
          sep = ""
        )

      if (
        (i %% 10) == 0
      ) {
        f_lines[id_x_line] <-
          a_line

        a_line <- ""

        id_x_line <-
          id_x_line + 1
      }
    }

    if (
      nchar(a_line) > 0
    ) {
      f_lines[id_x_line] <-
        a_line
    }

    # Open the file to be exported for writing
    dta_1 <-
      file(path, open = "wt")

    # Output the lines
    writeLines(f_lines, dta_1)

    # Close the file
    close(dta_1)

    return(TRUE)
  }
