#' @title common: A set of useful functions
#' @encoding UTF-8
#' @description The \strong{common} package is set of useful
#' functions that enhance base R.  These functions make up for
#' some deficiencies in the \strong{base} package.  For example,
#' there are functions to sort a data frame by multiple columns,
#' and manipulate data frame labels.  These are common activities,
#' and can be used in almost any R program.
#'
#' @section Functions Included:
#' The functions included in the \strong{common} package are
#' as follows:
#' \itemize{
#'   \item \code{\link{v}}: A generalized NSE quoting function.
#'   \item \code{\link{sort.data.frame}}: An overload of the sort
#'   function for data frames.
#'   \item \code{\link{labels.data.frame}}: An overload of the labels function
#'   for data frames.
#'   \item \code{\link{\%p\%}}: An infix operator for \code{paste0()}.
#'   \item \code{\link{\%eq\%}}: An enhanced equality infix operator.
#'   \item \code{\link{roundup}}: A rounding function that matches SAS® software.
#'   \item \code{\link{Sys.path}}: A function to return the path of the current program.
#'   \item \code{\link{file.find}}: A function to search for a file on the file system.
#'   \item \code{\link{dir.find}}: A function to search for directories on the file system.
#'   \item \code{\link{find.names}}: A function to search for variable names on a data frame.
#'   \item \code{\link{copy.attributes}}: A function to copy column attributes from one data
#'   frame to another.
#'   \item \code{\link{spaces}}: A function to create a string of blank spaces.
#'   \item \code{\link{supsc}}: A function to get UTF-8 superscript characters.
#'   \item \code{\link{subsc}}: A function to get UTF-8 subscript characters.
#'   \item \code{\link{symbol}}: A function to get UTF-8 symbol characters.
#'   \item \code{\link{changed}}: A function to create a string of spaces.
#'   \item \code{\link{source.all}}: A function to source an entire directory of R programs.
#' }
#' @keywords internal
#' @aliases common-package
#' @name common
"_PACKAGE"
