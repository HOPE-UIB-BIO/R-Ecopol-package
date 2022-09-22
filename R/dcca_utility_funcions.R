#' @title Parse line by tabs
#' @description
#' Takes a character string and parses it with TAB separator,
#' returning specified range of string entries
#' @author Petr Smilauer
util_parse_line_by_tabs <-
  function(a_line,
           id_x_from,
           id_x_to) {
    xy <-
      strsplit(a_line, "\t", fixed = T)[[1]]

    return(
      xy[id_x_from:id_x_to]
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
    a_line <-
      readLines(connection_file, n = num_lines)

    n_line <-
      length(a_line)

    if (
      n_line == 0
    ) {
      return("")
    }

    return(
      a_line[n_line]
    )
  }

#' @title Read individual line
#' @description
#' Takes an open connection to a text file and reads from it individual
#' lines until it arrives to a line starting with specified text
#' Found line is returned.
#' Passed connection (connection_file) must be already open for reading
#' The function returns empty string to indicate a failure
#' @author Petr Smilauer
util_read_to_line <-
  function(connection_file,
           sought_text) {
    a_line <-
      vector("character")

    while (TRUE) {
      a_line <-
        readLines(connection_file, n = 1)

      if (
        length(a_line) == 0
      ) {
        # return empty string on failure
        return("")
      }

      if (
        startsWith(a_line, sought_text)
      ) {
        break
      }
    }

    return(a_line[1])
  }
