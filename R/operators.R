


# Infix Operators ---------------------------------------------------------




#' @title An infix operator for \code{paste0()}
#' @description This function provides an infix operator for the
#' \code{\link{paste0}} function to concatenate strings.  The operator
#' will concatenate a vector of one or more values. The functionality
#' is identical to \code{paste0()}, but more convenient to use in some
#' situations.
#' @param x A value for the left side of the paste infix operator.
#' @param y A value for the right side of the paste infix operator.
#' @return The concatenated or pasted value.  No spaces will be inserted
#' in between the values to paste. If a vector of values is supplied,
#' a vector of pasted values will be returned.
#' @examples
#' # Paste together two strings
#' str <- "Hello" %p% "World"
#' str
#' # [1] "HelloWorld"
#'
#' # Paste together number and string
#' str <- 100 %p% " Kittens"
#' str
#' # [1] "100 Kittens"
#'
#' # Paste together two vectors
#' v1 <- c("A", "B", "C")
#' v2 <- c(1, 2, 3)
#' str <- v1 %p% v2
#' str
#' # [1] "A1" "B2" "C3"
#' @export
`%p%` <- function(x,y) {

  ret <- paste0(x,y)


  return(ret)
}



#' @title Check equality of two objects
#' @description The goal of the \code{\%eq\%} operator is to return a TRUE
#' or FALSE value when any two objects are compared.  The function provides a
#' simple, reliable equality check that allows comparing
#' of NULLs, NA values, and atomic data types without error.
#'
#' The function also allows comparing
#' of data frames.  It will return TRUE if all values in the
#' data frames are equal, and ignores differences in attributes.
#' @param x1 The first object to compare
#' @param x2 The second object to compare
#' @return A single TRUE or FALSE value depending on whether the objects are equal.
#' @examples
#' # Comparing of NULLs and NA
#' NULL %eq% NULL        # TRUE
#' NULL %eq% NA          # FALSE
#' NA %eq% NA            # TRUE
#' 1 %eq% NULL           # FALSE
#' 1 %eq% NA             # FALSE
#'
#' # Comparing of atomic values
#' 1 %eq% 1              # TRUE
#' "one" %eq% "one"      # TRUE
#' 1 %eq% "one"          # FALSE
#' 1 %eq% Sys.Date()     # FALSE
#'
#' # Comparing of vectors
#' v1 <- c("A", "B", "C")
#' v2 <- c("A", "B", "D")
#' v1 %eq% v1            # TRUE
#' v1 %eq% v2            # FALSE
#'
#' # Comparing of data frames
#' mtcars %eq% mtcars    # TRUE
#' mtcars %eq% iris      # FALSE
#' iris %eq% iris[1:50,] # FALSE
#'
#' # Mixing it up
#' mtcars %eq% NULL      # FALSE
#' v1 %eq% NA            # FALSE
#' 1 %eq% v1             # FALSE
#'
#' @export
`%eq%` <- function(x1, x2) {

  ret <- TRUE
  if (is.null(x1) & is.null(x2))
    ret <- TRUE
  else if (is.null(x1) | is.null(x2)) {
    ret <- FALSE
  } else if (all(typeof(x1) != typeof(x2))) {
    ret <- FALSE
  } else if ("data.frame" %in% class(x1)) {

    if (nrow(x1) != nrow(x2)) {
      ret <- FALSE
    } else if (ncol(x1) != ncol(x2)) {
      ret <- FALSE
    } else if (all(names(x1) != names(x2))) {
      ret <- FALSE
    } else {

      for (i in seq_along(x1)) {
        if (any(!strong_eq(x1[[i]], x2[[i]]))) {
          ret <- FALSE
          break
        }
      }
    }
  } else {

    if (length(x1) != length(x2)) {
      ret <- FALSE
    } else {

      if (any(!strong_eq(x1, x2))) {
        ret <- FALSE
      }
    }
  }

  return(ret)
}

#' @noRd
strong_eq <- Vectorize(function(x1, x2) {

  ret <- TRUE
  if (is.null(x1) & is.null(x2))
    ret <- TRUE
  else if (is.null(x1) & !is.null(x2))
    ret <- FALSE
  else if (!is.null(x1) & is.null(x2))
    ret <- FALSE
  else if (is.na(x1) & is.na(x2))
    ret <- TRUE
  else if (is.na(x1) & !is.na(x2))
    ret <- FALSE
  else if (!is.na(x1) & is.na(x2))
    ret <- FALSE
  else {
    ret <- x1 == x2

  }

  return(ret)

})


