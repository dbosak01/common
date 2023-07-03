

# File Operations ---------------------------------------------------------


# @title Returns the path of the current program
# @description A function that gets the full path of the currently running
# program.  If the function fails to retrieve the path for some reason,
# it will return a NULL.  The function takes no parameters.
# @returns The full path of the currently running program, or a NULL.
# @family fileops
# @examples
# # Get current path
# pth <- Sys.path()
# pth
# # [1] "C:/programs/myprogram.R"
# export
# Sys.path_back <- function() {
#
#   ppth <- Sys.path.internal()
#
#   return(ppth)
#
# }


# File Find ---------------------------------------------------------------



#' @title Search for files
#' @description A function to find files on the file system. The function
#' starts from the directory specified in the \code{path} parameter,
#' and searches outward in a radiating pattern
#' for the file name in the \code{pattern} parameter.
#' Results are returned as a vector of full paths in the order encountered.
#' The \code{up} and \code{down} parameters define the scope of the search.
#' This function has an advantage over \code{list.files} in that it can
#' search both up and down the file system, and limit the scope of the search.
#' @details
#' The \code{file.find} function attempts to find a file based on
#' a full or partial file name.  The file name is passed
#' on the \code{pattern} parameter.
#' The \code{pattern} accepts both the single character question mark
#' wild card (?), and the asterisk multi-character wild card (*).
#' Searches are case-insensitive.
#'
#' Starting from the base path specified in the \code{path} parameter,
#' the function searches
#' both above and below the base path in an alternating
#' pattern.  The function will first search the base path, then up one level,
#' then down one level, and so on.  The boundaries of
#' the search can be controlled by the \code{up} and \code{down} parameters.
#'
#' You can control whether or not you want files from the base directory
#' included in the results.  To include these files, ensure both \code{up}
#' and \code{down} parameters are zero or greater.  If either of these
#' parameters is set to -1, the base path will be excluded.  For example,
#' \code{up = 3, down = 1} will search up three levels, and down one level.
#' \code{up = 3, down = 0} will search up three levels and not search down,
#' but will include the base directory.  \code{up = 3, down = -1} will search
#' up three levels, not search down, and not include the base directory in
#' the results.
#' @param path  The directory to start searching from.  Default is the current
#' working directory.
#' @param pattern A full or partial name of the file to find.  If partial,
#' use the question mark (?) or asterisk (*) characters to indicate
#' the missing piece. Default is NULL, which will return all files.
#' @param up The number of levels above the base path to search. A value of
#' zero (0) means that you do not want to search up.  A value of -1 means
#' you do not want to include the base directory in the search results.
#' Default is 3 levels up.
#' @param down The number of levels below the base path to search. A value of
#' zero (0) means that you do not want to search down.  A value of -1 means you
#' do not want to include the base directory in the search results.
#' Default is 1 level down.
#' @returns A vector of one or more full file paths that met the search criteria.
#' The paths in the vector are returned
#' in the order of matches, according to the search algorithm.  That means
#' the first file found will be in position one, and the last file found
#' will be at the end of the vector. A NULL is returned if no
#' files met the search criteria.
#' @family fileops
#' @examples
#' # Search for a file named "globals.R"
#' file.find(getwd(), "globals.R")
#'
#' # Search for Rdata files
#' file.find(getwd(), "*.Rdata")
#'
#' # Search for Rdata files up only
#' file.find(getwd(), "*.Rdata", up = 3, down = 0)
#'
#' # Search for Rdata files up only, skipping the current working directory
#' file.find(getwd(), "*.Rdata", up = 3, down = -1)
#'
#' @import utils
#' @export
file.find <- function(path = ".", pattern = NULL, up = 3, down = 1) {

  if (is.null(up)) {
    stop("The 'up' parameter cannot be NULL.")
  }

  if (up < -1) {
    stop("The 'up' parameter cannot be less than -1.")
  }

  if (is.null(down)) {
    stop("The 'down' parameter cannot be NULL.")
  }

  if (down < -1) {
    stop("The 'up' parameter cannot be less than -1.")
  }

  ret <- NULL

  pth <- normalizePath(path)

  if (is.null(pattern))
    srch <- NULL
  else
    srch <- glob2rx(pattern)


  # Examine base directory
  if (up > -1 & down > -1)
    ret <- list_files(pth, srch)
  else
    ret <- c()

  mx <- max(up, down)

  # Search directories above and below
  if (mx > 0) {
    for (i in seq(1, mx)) {

      if (i <= up) {

        fls <- list_files(get_dir_up(pth, i), srch)
        ret <- append(ret, fls)

      }

      if (i <= down) {

        fls <- list_files(get_dirs_down(pth, i), srch)
        ret <- append(ret, fls)

      }
    }
  }


  return(ret)

}

#' @noRd
get_dir_up <- function(path, offset) {

  ret <- path

  for (i in seq(1, offset)) {

    ret <- dirname(ret)

  }

  return(ret)
}

#' @noRd
get_dirs_down <- function(path, offset) {

  prnts <- path
  ret <- c()

  if (offset > 0) {

    for (i in seq(1, offset)) {


      lst <- list.dirs(prnts, full.names = TRUE, recursive = FALSE)

      if (i == offset) {
        ret  <- append(ret, lst)

      } else {

        prnts <- lst

      }

    }

  }


  return(ret)

}

#' @noRd
list_files <- function(path = NULL, pattern = NULL){

  ret <- NULL

  # Get list of all files and folders that match
  ret <- list.files(path = path, pattern = pattern, all.files = FALSE, full.names = TRUE,
                    recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE,
                    no.. = TRUE)

  # Exclude directories
  ret <- ret[!dir.exists(ret)]

  if (length(ret) == 0)
    ret <- NULL


  return(ret)

}


# Dir Find ----------------------------------------------------------------


#' @title Search for directories
#' @description A function to find directories on the file system. The function
#' starts from the directory specified in the \code{path} parameter,
#' and searches outward in a radiating pattern
#' for the ending directory name in the \code{pattern} parameter.
#' The \code{up} and \code{down} parameters define the scope of the search.
#' Results are returned as a vector of full paths in the order encountered.
#' This function has an advantage over \code{list.dirs} in that it can
#' search both up and down the file system, and limit the scope of the search.
#' @details
#' The \code{dir.find} function attempts to find a directory based on
#' a full or partial directory name.  The directory name is passed
#' on the \code{pattern} parameter.
#' The \code{pattern} accepts both the single character question mark
#' wild card (?), and the asterisk, multi-character wild card (*).
#' Searches are case-insensitive.
#'
#' Starting from the base path specified in the \code{path} parameter,
#' the function searches
#' both above and below the base path in an alternating
#' pattern.  The function will first search the base path, then up one level,
#' then down one level, and so on.  The boundaries of
#' the search can be controlled by the \code{up} and \code{down} parameters.
#'
#' You can control whether or not you want the base directory
#' included in the results.  To include this directory, ensure both \code{up}
#' and \code{down} parameters are zero or greater.  If either of these
#' parameters is set to -1, the base path will be excluded.  For example,
#' \code{up = 3, down = 1} will search up three levels, and down one level.
#' \code{up = 3, down = 0} will search up three levels and not search down,
#' but will include the base directory.  \code{up = 3, down = -1} will search
#' up three levels, not search down, and not include the base directory in
#' the results.
#' @param path  The directory to start searching from.  Default is the current
#' working directory.
#' @param pattern A full or partial name of the directory to find.  If partial,
#' use the question mark (?) or asterisk (*) characters to indicate
#' the missing piece. Default is NULL, which will return all directories.
#' @param up The number of levels above the base path to search. A value of
#' zero (0) means that you do not want to search up.  A value of -1 means
#' you do not want to include the base directory in the search results.
#' Default is 5 levels up.
#' @param down The number of levels below the base path to search. A value of
#' zero (0) means that you do not want to search down.  A value of -1 means you
#' do not want to include the base directory in the search results.
#' Default is 2 levels down.
#' @returns A vector of one or more full directory paths that met
#' the search criteria. The paths in the vector are returned
#' in the order of matches, according to the search algorithm.  That means
#' the first directory found will be in position one, and the last
#' directory found
#' will be at the end of the vector. A NULL is returned if no
#' directories met the search criteria.
#' @family fileops
#' @examples
#' # Search for a directory named "prod"
#' file.find(pattern = "prod")
#'
#' # Search for a directory that starts with "dat"
#' file.find(pattern = "dat*")
#'
#' # Search for a directory up only
#' file.find(pattern = "dat*", up = 3, down = 0)
#'
#' # Search for directories up only, skipping the current working directory
#' file.find(pattern = "dat*", up = 3, down = -1)
#'
#' @import utils
#' @export
dir.find <- function(path = ".", pattern = NULL, up = 5, down = 2) {

  if (is.null(up)) {
    stop("The 'up' parameter cannot be NULL.")
  }

  if (up < -1) {
    stop("The 'up' parameter cannot be less than -1.")
  }

  if (is.null(down)) {
    stop("The 'down' parameter cannot be NULL.")
  }

  if (down < -1) {
    stop("The 'up' parameter cannot be less than -1.")
  }

  ret <- NULL

  pth <- normalizePath(path)

  if (is.null(pattern))
    srch <- NULL
  else
    srch <- glob2rx(pattern)


  # Examine base directory
  if (up > -1 & down > -1) {

    if (length(get_matching_dirs(pth, srch)) > 0)
      ret <- pth
  } else
    ret <- c()


  mx <- max(up, down)

  # Search directories above and below
  if (mx > 0) {
    for (i in seq(1, mx)) {

      if (i <= up) {

        drs <- get_matching_dirs(get_dir_up(pth, i), srch)
        ret <- append(ret, drs)

      }

      if (i <= down) {

        if (i == 1) {
          drs <- list_dirs(pth, srch)
        } else {
          drs <- list_dirs(get_dirs_down(pth, i - 1), srch)
        }

        ret <- append(ret, drs)

      }
    }
  }

  if (length(ret) == 0)
    ret <- NULL
  else
    ret <- ret[!duplicated(ret)]


  return(ret)

}

#' @noRd
list_dirs <- function(path = NULL, pattern = NULL){

  ret <- NULL

  # Get a list of dirs in path
  ret <- list.dirs(path, recursive = FALSE, full.names = TRUE)

  # Filter by matching base names
  ret <- get_matching_dirs(ret, pattern)

  if (length(ret) == 0)
    ret <- NULL


  return(ret)

}

get_matching_dirs <- function(dirs, srch = NULL) {

  ret <- dirs

  if (!is.null(srch)) {

    nms <- basename(dirs)

    mtch <- grepl(srch, nms, ignore.case = TRUE)

    ret <- ret[mtch]

  }

  return(ret)

}
