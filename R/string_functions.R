

# String Manipulation -----------------------------------------------------



#' @title Creates a string of blank spaces
#' @description A function to create a string of some number of blank spaces.
#' This function is useful when trying to align things.
#' @param num The desired number of spaces.
#' @returns A single character vector of blank spaces.
#' @family strings
#' @examples
#' # Create spaces
#' spaces(10)
#' # [1] "          "
#'
#' # Use spaces to separate something
#' str <- "Left" %p% spaces(40) %p% "Right"
#' str
#' # [1] "Left                                        Right"
#' @export
spaces <- function(num) {

 ret <-  paste0(rep(" ", num), collapse = "")

 return(ret)

}


