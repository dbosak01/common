



# Sys.path.internal <- function() {
#
#   ppth <- NULL
#
#   tryCatch({
#
#     # if (utils::packageVersion("this.path")  >= "2.0.0")
#     #   ppth <- this.path::sys.path()
#     # else {
#       ppth <- this.path::this.path()
#
#     # }
#
#   }, error = function(e) { ppth <- NULL})
#
#
#   return(ppth)
#
# }




#' @title Returns the path of the current program
#' @description A function that gets the full path of the currently running
#' program.  If the function fails to retrieve the path for some reason,
#' it will return a NULL.  The function takes no parameters.
#' @returns The full path of the currently running program, or a NULL.
#' @family fileops
#' @examples
#' # Get current path
#' pth <- Sys.path()
#' pth
#' # [1] "C:/programs/myprogram.R"
#' @export
Sys.path <- function() {

  ret <- NULL

  debug <- FALSE
  if (!is.null(options()[["common.debug"]])) {
    debug <- options("common.debug")[[1]]
  }

  # Get list of installed packages
  si <- sessionInfo()
  pkglst <- names(si$otherPkgs)

  # Assign debugSource.  This is used when running RStudio in debug mode.
  debugSource <- if (.Platform$GUI == "RStudio")
    get("debugSource", "tools:rstudio", inherits = FALSE)


  # Check call stack to see if this program is being sourced in some way.
  # There are several different sourcing functions.  If one of them is called,
  # pull the file name from the appropriate variable inside that function.
  for (n in seq.int(to = 1, by = -1, length.out = sys.nframe() - 1)) {

    if (identical(sys.function(n), base::source)) {
      if (exists("ofile", envir = sys.frame(n), inherits = FALSE)) {

        ret <- get("ofile", envir = sys.frame(n), inherits = FALSE)

        if (!is.character(ret))
          ret <- summary.connection(ret)$description

      }

      if (debug)
        message("base::source() call identified:" %p% ret)

    }

    if (identical(sys.function(n), base::sys.source)) {

      if (exists("exprs", envir = sys.frame(n), inherits = FALSE)) {

        ret <- get("file", envir = sys.frame(n), inherits = FALSE)

      }

      if (debug)
        message("base::sys.source() call identified:" %p% ret)

    }

    if ("testthat" %in% pkglst) {
      if (identical(sys.function(n), testthat::source_file)) {

        if (exists("path", envir = sys.frame(n), inherits = FALSE)) {

          ret <- get("path", envir = sys.frame(n), inherits = FALSE)

        }

        if (debug)
          message("testthat::source_file() call identified:" %p% ret)

      }
    }

    if ("rmarkdown" %in% pkglst) {
      if (identical(sys.function(n), rmarkdown::render)) {

        if (exists("input", envir = sys.frame(n), inherits = FALSE)) {

          ret <- get("input", envir = sys.frame(n), inherits = FALSE)

        }

        if (debug)
          message("rmarkdown::render() call identified:" %p% ret)

      }
    }

    if ("knitr" %in% pkglst) {
      if (identical(sys.function(n), knitr::knit)) {

        if (exists("input", envir = sys.frame(n), inherits = FALSE)) {

          ret <- get("input", envir = sys.frame(n), inherits = FALSE)

        }

        if (debug)
          message("knitr::knit() call identified:" %p% ret)

      }
    }

    if ("compiler" %in% pkglst) {
      if (identical(sys.function(n), compiler::loadcmp)) {

        if (exists("file", envir = sys.frame(n), inherits = FALSE)) {

          ret <- get("file", envir = sys.frame(n), inherits = FALSE)

        }

        if (debug)
          message("compiler::loadcmp() call identified:" %p% ret)
      }
    }

    if ("box" %in% pkglst) {
      if (identical(sys.function(n), box::use)) {
        tryCatch({

          ret <- box::file(mustWork = FALSE)

        }, error = function(e) {ret <- NULL})

        if (debug)
          message("box::use() call identified:" %p% ret)

      }
    }

    if (identical(sys.function(n), debugSource)) {

      tryCatch({

        ret <- get("fileName", envir = sys.frame(n), inherits = FALSE)

      }, error = function(e) {ret <- NULL})

      if (debug)
        message("debugSource() call identified:" %p% ret)

    }

    # If assigned above, bail out of for loop
    if (!is.null(ret)) {

      if (debug)
        message("source call found:" %p% ret)

      break()

    }
  }

  # If no source call, check for interactive session or command line
  if (is.null(ret)) {

    # Running from RStudio
    if (.Platform$GUI == "RStudio") {


      # This variable contains information about the currently open document
      tryCatch({

        context <- get(".rs.api.getSourceEditorContext",
                     "tools:rstudio", inherits = FALSE)()

        if (!is.null(context)) {

          ret <- context[["path"]]

          if (nzchar(ret)) {
            Encoding(ret) <- "UTF-8"

          }
        }
      }, error = function(e) {ret <- NULL})

      if (debug)
        message("RStudio interactive call found:" %p% ret)
    }

    # Check for shells
    else if (.Platform$OS.type == "windows" && .Platform$GUI == "RTerm" ||
        .Platform$OS.type == "unix"    && .Platform$GUI == "X11")
    {

      # Parse command line to pull out file argument
      argv <- paste(commandArgs(), collapse = " ")
      startpos <- regexpr("--args", argv, fixed = TRUE)
      if (startpos > 0) {
        argv <- substr(argv, 1, startpos - 1)

      }

      ret <- trimws(sub("--file=", "", argv, fixed = TRUE))

      if (length(ret) == 0)
        ret <- NULL

      if (debug)
        message("Shell call found:" %p% ret)

    }

    # Running from RGui on Windows
    else if (.Platform$OS.type == "windows" && .Platform$GUI == "Rgui") {

      if (debug)
        message("Windows Rgui call found:" %p% ret)

    }

    # Running from RGui on macOS
    else if (.Platform$OS.type == "unix" && .Platform$GUI == "AQUA") {

      if (debug)
        message("Unix Rgui call found:" %p% ret)

    }

  }

  # Clean up and normalize path
  if (!is.null(ret)) {

    if (debug)
      message("Path found:" %p% ret)

    ret <- sub("file:///", "", ret, fixed = TRUE)

    # Try to normalize. If can't, just ignore
    ret <- normalizePath(ret, "/", FALSE)

  } else {


    if (debug)
      message(paste0("Path not found. Platform: ", .Platform$OS.type,
                     " GUI: ", .Platform$GUI))

  }


  return(ret)

}





