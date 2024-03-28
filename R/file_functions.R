

# File Operations ---------------------------------------------------------




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



# Source All --------------------------------------------------------------

#' @title Source all programs in a directory
#' @description A function to source all programs in a specified directory.
#' The function will run each R program file in the directory, and then
#' return a data frame of results of the run.
#' @details
#' The \code{source.all} function attempts to run all programs in a directory.
#' This function is useful for batch runs.  It has parameters to control
#' which programs are run or not run.  By default, the function will run
#' all programs in the working directory.  You can use the "pattern" and
#' "exclude" parameters to specify individual program names, or wild card matches.
#' Inclusion and exclusion patterns are case-insensitive.
#'
#' Note that the function will run all programs, regardless of any errors.
#' Errors will be indicated in the "Status" and "Message" columns of the result
#' dataset.
#' @section Result Dataset:
#' The \code{source.all} function returns a dataset showing the results of the
#' source operation. There will be one row for each program executed. The
#' return dataset has the following columns:
#' \itemize{
#'   \item{\strong{Filename}: The name of the program.}
#'   \item{\strong{StartTime}: The date and time execution started.}
#'   \item{\strong{EndTime}: The date and time execution ended.}
#'   \item{\strong{Status}: A numeric value indicating whether or not
#'   the program returned errors or warnings. A zero (0) value indicates that
#'   no errors occurred. A one (1) value indicates that an error
#'   occurred. Warnings can also be generated along with an error, but
#'   the status will still be one (1). A two (2) value indicates
#'   that warnings occurred but no errors. Note that capture of warnings
#'   is less reliable than the capture of errors. It is possible that a program
#'   may generate a warning and still return a zero (0) status. If you want to
#'   ensure that warnings are detected, convert them to errors with
#'   \code{options(warn = 2)}.}
#'   \item{\strong{Message}: If errors or warnings are returned from the program,
#'   they will be shown in this column. Multiple messages will be separated
#'   with a semi-colon (;) and a carriage return.}
#' }
#' In addition to the information shown above, the results dataset will have
#' attributes assigned with the parameter values passed to the function. Those
#' attributes can be observed with the Base R \code{attributes()} function.
#' @section Source Isolation:
#' Multiple programs running in the same environment have a risk of conflicting
#' variables or data. Variables created by the first program can possibly interfere
#' with the running of the next program.  Or they could conflict with variables
#' in the global environment. To avoid such conflicts, each program
#' is run in its own environment by default. \code{isolate = TRUE} starts each program
#' with a clean workspace, and is the best choice for running programs in
#' batch.
#'
#' There may be situations, however, where you do not want to isolate the
#' source calls.  For example, if you are loading functions from a utility
#' library, you may actually wanted them loaded into the global environment
#' so they can by accessed by you or your programs.  In this case, set the
#' "isolate" parameter to FALSE.
#'
#' Lastly, there may be situations where you want to intentionally share an
#' environment, or extract values create by the running programs.
#' In this case, you can instantiate a new environment
#' yourself, and pass that to the "isolate" parameter instead of TRUE or FALSE.
#' Note that this environment will be shared by all programs, but will not have
#' access to the global environment.
#' @param path  The directory to source programs from.  Default is the current
#' working directory.
#' @param pattern A full or partial name of the programs to source.  If partial,
#' use the question mark (?) or asterisk (*) characters to indicate
#' the missing piece(s). Default is NULL, which will return all programs.
#' You may pass multiple patterns as a vector.  In that case, the function
#' will perform an "or" operation on each pattern. Note that it is not necessary
#' to include the ".R" file extension in your patterns. It is assumed that all
#' source files have a ".R" extension.
#' @param exclude A vector of patterns to exclude from the included programs.
#' The exclusion patterns can be the names of specific programs or a wild card
#' exclusion. The asterisk (*)
#' and question mark (?) characters may be used to indicate partial matches.
#' Similar to the "pattern" parameter, the ".R" file extension can be ignored.
#' @param isolate Whether to isolate each source call to its own environment.
#' Valid values are TRUE, FALSE, or an environment to run in.  If the isolate
#' parameter is FALSE, the programs will run in the global environment. Default
#' is TRUE.
#' @returns A data frame of the results of the source operation. The
#' data frame will show each file sourced, the time started, the time ended,
#' the status, and any error messages. The status value is either 0 (no errors)
#' or 1 (errors).
#' @family fileops
#' @examples
#' # Create temp directory
#' tmp <- tempdir()
#'
#' # Write program 1
#' p1 <- file(file.path(tmp, "prog1.R"))
#' writeLines("print('Hello from program 1')", p1)
#' close(p1)
#'
#' # Write program 2
#' p2 <- file(file.path(tmp, "prog2.R"))
#' writeLines("stop('Error from program 2')", p2)
#' close(p2)
#'
#' # Write program 3
#' p3 <- file(file.path(tmp, "prog3.R"))
#' writeLines("print('Hello from program 3')", p3)
#' close(p3)
#'
#' # Example #1: Run all programs
#' res1 <- source.all(tmp)
#' # [1] "Hello from program 1"
#' # [1] "Hello from program 3"
#'
#' # View results
#' res1
#' #   Filename           StartTime             EndTime Status              Message
#' # 1  prog1.R 2024-03-05 10:12:04 2024-03-05 10:12:04      0              Success
#' # 2  prog2.R 2024-03-05 10:12:04 2024-03-05 10:12:04      1 Error from program 2
#' # 3  prog3.R 2024-03-05 10:12:04 2024-03-05 10:12:04      0              Success
#'
#' #' # Example #2: Exclusion criteria
#' res2 <- source.all(tmp, exclude = "prog2")
#' # [1] "Hello from program 1"
#' # [1] "Hello from program 3"
#'
#' # View results
#' res2
#' # Filename           StartTime             EndTime Status Message
#' # 1  prog1.R 2024-03-05 10:13:24 2024-03-05 10:13:24      0 Success
#' # 2  prog3.R 2024-03-05 10:13:24 2024-03-05 10:13:24      0 Success
#'
#' # Example #3: Inclusion criteria
#' res3 <- source.all(tmp, pattern = "*2")
#'
#' # View results
#' res3
#' #   Filename           StartTime             EndTime Status              Message
#' # 1  prog2.R 2024-03-05 10:16:41 2024-03-05 10:16:41      1 Error from program 2
#'
#' # View attributes
#' attributes(res3)
#' # $names
#' # [1] "Filename"  "StartTime" "EndTime"   "Status"    "Message"
#' #
#' # $class
#' # [1] "data.frame"
#' #
#' # $row.names
#' # [1] 1
#' #
#' # $path
#' # [1] "C:\\Users\\dbosa\\AppData\\Local\\Temp\\RtmpGAXYJl"
#' #
#' # $pattern
#' # [1] "*2.R"
#' #
#' # $errors
#' # [1] 1
#' @export
source.all <- function(path = ".", pattern = NULL, exclude = NULL,
                       isolate = TRUE) {


  if (!dir.exists(path)) {

    stop(paste("Specified path does not exist:", path))
  }

  # Append .R extension if not supplied
  if (is.null(pattern)) {
    pattern <- "*.R"

  } else {

    mpt <- grepl(".r", tolower(pattern), fixed = TRUE)
    if (any(mpt == FALSE)) {
      for (i in seq_len(length(mpt))) {
        if (mpt[i] == FALSE) {
          pattern[i] <- paste0(pattern[i], ".R")
        }
      }
    }
  }

  fnms <- c()
  # If there are multiple patterns, search for each one
  for (i in seq_len(length(pattern))) {

    tnms <- file.find(path, pattern = pattern[i], up = 0, down = 0)

    fnms <- append(fnms, tnms)

  }

  # Remove any duplicates
  fnms <- unique(fnms)

  # Perform exclusions
  if (!is.null(fnms) & !is.null(exclude)) {

    excl <- glob2rx(exclude)

    for (ex in excl) {
      tmpnms <- fnms
      fnms <- c()
      for (j in seq_len(length(tmpnms))) {
        bnm <- basename(tmpnms[j])
        tnm <- substring(bnm, 1, nchar(bnm) - 2)

        if (!grepl(ex, tnm, ignore.case = TRUE)) {
          fnms <- append(fnms, tmpnms[j])
        }
      }
    }

  }

  # Create vars for return dataset
  nms <- c()
  st <- c()
  en <- c()
  stat <- c()
  msgs <- c()

  for (fnm in fnms) {

    # Create new environment for each program
    if (is.environment(isolate)) {
      e <- isolate
    } else if (isolate == FALSE) {
      e <- globalenv()
    } else {
      e <- new.env()
    }

    # Capture program name and start time
    nms <- append(nms, basename(fnm))
    st <- append(st, Sys.time())

    tres <- tryCatch({
      source(fnm, local = e)
      # sys.source(fnm, envir = e, toplevel.env = globalenv())
      NULL
    }, error = function(cond) {
      # Call existing error handler

      errfnc <- eval(getOption("error"), envir = e)
      if (!is.null(errfnc))
        eval(errfnc, envir = e)

        # Return error messages
        geterrmessage()
    })

    lw1 <- eval(warnings(), envir = e)

    lw2 <- eval(getOption("logr.warnings"), envir = e)

    lw <- unique(append(lw1, lw2))

    # Capture status and any errors or warnings
    if (!is.null(tres)) {
      if (length(lw) > 0) {
        stat <- append(stat, 1)
        msgs <- append(msgs, paste0(tres, ';\n', lw, collapse = ';\n'))

      } else {
        stat <- append(stat, 1)
        msgs <- append(msgs, paste0(tres, collapse=";\n"))
      }
    } else {

      if (length(lw) > 0) {
        stat <- append(stat, 2)
        msgs <- append(msgs, paste0(lw, collapse = ';\n'))

      } else {
        stat <- append(stat, 0)
        msgs <- append(msgs, "Success")
      }
    }

    # Capture end time
    en <- append(en, Sys.time())

  }

  # Construct return data
  if (length(nms) == 0) {

    ret <- data.frame(Filename = "", StartTime = Sys.time(),
                      EndTime = Sys.time(), Status = 0,
                      Message = "", stringsAsFactors = FALSE)

    ret <- ret[0, ]

  } else {
    ret <- data.frame(Filename = nms, StartTime = st,
                      EndTime = en, Status = stat,
                      Message = msgs, stringsAsFactors = FALSE)
  }


  attr(ret, "path") <- path
  attr(ret, "pattern") <- pattern
  attr(ret, "exclude") <- exclude
  attr(ret, "errors") <- length(stat[stat == 1])

  return(ret)
}


