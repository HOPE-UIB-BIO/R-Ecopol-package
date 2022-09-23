#' @title Parse line by tabs
#' @description
#' Takes a character string and parses it with TAB separator,
#' returning specified range of string entries
#' @author Petr Smilauer
util_get_line_by_tabs <-
  function(sel_line,
           id_from,
           id_to) {
    xy <-
      strsplit(sel_line, "\t", fixed = T)[[1]]

    return(
      xy[id_from:id_to]
    )
  }

#' @title Read number of lines
#' @description
#' Reads specified number of lines from the passed
#' connection. Last read line is returned
#' @author Petr Smilauer
util_read_n_lines <-
  function(connection_file,
           num_lines) {
    sel_line <-
      readLines(connection_file, n = num_lines)

    n_line <-
      length(sel_line)

    if (
      n_line == 0
    ) {
      return("")
    }

    return(
      sel_line[n_line]
    )
  }

#' @title Read file until found selected text
#' @description
#' Takes an open connection to a text file and reads from it individual
#' lines until it arrives to a line starting with specified text
#' Found line is returned.
#' Passed connection (connection_file) must be already open for reading
#' The function returns empty string to indicate a failure
#' @author Petr Smilauer
util_read_lines_until <-
  function(connection_file,
           sel_text) {
    sel_line <-
      vector("character")

    while (TRUE) {
      sel_line <-
        readLines(connection_file, n = 1)

      if (
        length(sel_line) == 0
      ) {
        # return empty string on failure
        return("")
      }

      if (
        startsWith(sel_line, sel_text)
      ) {
        break
      }
    }

    return(sel_line[1])
  }

#' @title Delete files within folder
#' @description Try to delete all files within folder but `file_to_omit`
util_clean_files <-
  function(sel_folder_path,
           file_to_omit = "canoco.exe") {
    all_files <-
      list.files(sel_folder_path, full.names = TRUE)

    files_filtered <-
      all_files[stringr::str_detect(all_files, file_to_omit, negate = TRUE)]

    for (i in seq_along(files_filtered)) {
      suppressWarnings(
        try(
          file.remove(files_filtered[i]),
          silent =  TRUE
        )
      )
    }
  }
