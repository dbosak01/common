


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
#' @family operators
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
#' @family operators
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




#' @title Check for inequality of two objects
#' @description The goal of the \code{\%ne\%} operator is to return a TRUE
#' or FALSE value when any two objects are compared.  The function is the
#' opposite of the equality operator. It returns a TRUE when the objects
#' are not equal.
#'
#' This operator also allows comparing
#' of data frames.  It will return TRUE if any values in the
#' data frames are not equal, and ignores differences in attributes.
#' @param x1 The first object to compare
#' @param x2 The second object to compare
#' @return A single TRUE or FALSE value depending on whether the objects are
#' not equal.
#' @examples
#' # Comparing of NULLs and NA
#' NULL %ne% NULL        # FALSE
#' NULL %ne% NA          # TRUE
#' NA %ne% NA            # FALSE
#' 1 %ne% NULL           # TRUE
#' 1 %ne% NA             # TRUE
#'
#' # Comparing of atomic values
#' 1 %ne% 1              # FALSE
#' "one" %ne% "one"      # FALSE
#' 1 %ne% "one"          # TRUE
#' 1 %ne% Sys.Date()     # TRUE
#'
#' # Comparing of vectors
#' v1 <- c("A", "B", "C")
#' v2 <- c("A", "B", "D")
#' v1 %ne% v1            # FALSE
#' v1 %ne% v2            # TRUE
#'
#' # Comparing of data frames
#' mtcars %ne% mtcars    # FALSE
#' mtcars %ne% iris      # TRUE
#' iris %ne% iris[1:50,] # TRUE
#'
#' # Mixing it up
#' mtcars %ne% NULL      # TRUE
#' v1 %ne% NA            # TRUE
#' 1 %ne% v1             # TRUE
#' @family operators
#' @export
`%ne%` <- function(x1, x2) {


  ret <- !(x1 %eq% x2)


  return(ret)
}



#' @title Perform less than or equal comparison between two objects
#' @description The goal of the comparison operators is to return a TRUE
#' or FALSE value when any two objects are compared.  The operators provides a
#' simple, reliable equality check that allows comparing
#' of NULLs, NA values, and atomic data types without error. This operator
#' performs a less than or equal to comparison.
#'
#' For data frames, the operator will compare all values in all columns, and return
#' a single TRUE if all values in the second data frame are less than or equal to
#' the corresponding values in the first data frame.
#' @details
#' Additional details...
#' @param x1 The first object to compare
#' @param x2 The second object to compare
#' @return A single TRUE or FALSE value indicating the results of the comparison.
#' @examples
#' # Comparing of NULLs and NA
#' NULL %le% NULL        # TRUE
#' NULL %le% NA          # FALSE
#' NA %le% NA            # TRUE
#' 1 %le% NULL           # FALSE
#' 1 %le% NA             # FALSE
#'
#' # Comparing of atomic values
#' 1 %le% 1              # TRUE
#' 2 %le% 1              # FALSE
#' 1 %le% 2              # TRUE
#' "one" %le% "one"      # TRUE
#' 1 %le% "one"          # FALSE
#' 1 %le% Sys.Date()     # TRUE  (Sys.Date() is a number)
#'
#' # Comparing of vectors
#' v1 <- c(0, 1, 2)
#' v2 <- c(1, 2, 3)
#' v3 <- c(2, 3, 4)
#' v1 %le% v1            # TRUE
#' v1 %le% v2            # TRUE
#' v2 %le% v1            # FALSE
#' v2 %le% v3            # TRUE
#'
#' # Comparing of data frames
#' d1 <- data.frame(A = v1, B = v2)
#' d2 <- data.frame(A = v2, B = v3)
#' d1 %le% d1            # TRUE
#' d1 %le% d2            # TRUE
#' d2 %le% d1            # FALSE
#'
#' # Mixing it up
#' d1 %le% NULL          # FALSE
#' v1 %le% d1            # FALSE
#' 1 %le% v1             # FALSE
#' @family operators
#' @export
`%le%` <- function(x1, x2) {

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
        if (any(!strong_le(x1[[i]], x2[[i]]))) {
          ret <- FALSE
          break
        }
      }
    }

  } else {

    if (length(x1) != length(x2)) {
      ret <- FALSE
    } else {

      if (any(!strong_le(x1, x2))) {
        ret <- FALSE
      }
    }
  }

  return(ret)
}

#' @noRd
strong_le <- Vectorize(function(x1, x2) {

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
    ret <- x1 <= x2

  }

  return(ret)

})

#' @title Perform greater than or equal comparison between two objects
#' @description The goal of the comparison operators is to return a TRUE
#' or FALSE value when any two objects are compared.  The operators provides a
#' simple, reliable equality check that allows comparing
#' of NULLs, NA values, and atomic data types without error. This operator
#' performs a greater than or equal comparison.
#'
#' For data frames, the operator will compare all values in all columns, and return
#' a single TRUE if all values in the second data frame are greater than or equal
#' to the corresponding values in the first data frame.
#' @details
#' Additional details...
#' @param x1 The first object to compare
#' @param x2 The second object to compare
#' @return A single TRUE or FALSE value indicating the results of the comparison.
#' @examples
#' # Comparing of NULLs and NA
#' NULL %ge% NULL        # TRUE
#' NULL %ge% NA          # FALSE
#' NA %ge% NA            # TRUE
#' 1 %ge% NULL           # FALSE
#' 1 %ge% NA             # FALSE
#'
#' # Comparing of atomic values
#' 1 %ge% 1              # TRUE
#' 2 %ge% 1              # TRUE
#' 1 %ge% 2              # FALSE
#' "one" %ge% "one"      # TRUE
#' 1 %ge% "one"          # FALSE
#' 1 %ge% Sys.Date()     # FALSE
#' Sys.Date() %ge% 1     # TRUE (Sys.Date() is a number)
#'
#' # Comparing of vectors
#' v1 <- c(0, 1, 2)
#' v2 <- c(1, 2, 3)
#' v3 <- c(2, 3, 4)
#' v1 %ge% v1            # TRUE
#' v1 %ge% v2            # FALSE
#' v2 %ge% v1            # TRUE
#' v3 %ge% v1            # TRUE
#'
#' # Comparing of data frames
#' d1 <- data.frame(A = v1, B = v2)
#' d2 <- data.frame(A = v2, B = v3)
#' d1 %ge% d1            # TRUE
#' d1 %ge% d2            # FALSE
#' d2 %ge% d1            # TRUE
#'
#' # Mixing it up
#' d1 %ge% NULL          # FALSE
#' v1 %ge% d1            # FALSE
#' 1 %ge% v1             # FALSE
#' @family operators
#' @export
`%ge%` <- function(x1, x2) {

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
        if (any(!strong_ge(x1[[i]], x2[[i]]))) {
          ret <- FALSE
          break
        }
      }
    }

  } else {

    if (length(x1) != length(x2)) {
      ret <- FALSE
    } else {

      if (any(!strong_ge(x1, x2))) {
        ret <- FALSE
      }
    }
  }

  return(ret)
}

#' @noRd
strong_ge <- Vectorize(function(x1, x2) {

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
    ret <- x1 >= x2

  }

  return(ret)

})



#' @title Perform less than comparison between two objects
#' @description The goal of the comparison operators is to return a TRUE
#' or FALSE value when any two objects are compared.  The operators provides a
#' simple, reliable equality check that allows comparing
#' of NULLs, NA values, and atomic data types without error. This operator
#' performs a less than comparison.
#'
#' For data frames, the operator will compare all values in all columns, and return
#' a single TRUE if all values in the second data frame are less than the
#' corresponding values in the first data frame.
#' @param x1 The first object to compare
#' @param x2 The second object to compare
#' @return A single TRUE or FALSE value indicating the results of the comparison.
#' @examples
#' # Comparing of NULLs and NA
#' NULL %lt% NULL        # FALSE
#' NULL %lt% NA          # FALSE
#' NA %lt% NA            # FALSE
#' 1 %lt% NULL           # FALSE
#' 1 %lt% NA             # FALSE
#'
#' # Comparing of atomic values
#' 1 %lt% 1              # FALSE
#' 2 %lt% 1              # FALSE
#' 1 %lt% 2              # TRUE
#' "one" %lt% "one"      # FALSE
#' 1 %lt% "one"          # FALSE
#' 1 %lt% Sys.Date()     # TRUE (Sys.Date() is a number)
#'
#' # Comparing of vectors
#' v1 <- c(0, 1, 2)
#' v2 <- c(1, 2, 3)
#' v3 <- c(2, 3, 4)
#' v1 %lt% v1            # FALSE
#' v1 %lt% v2            # TRUE
#' v2 %lt% v1            # FALSE
#' v2 %lt% v3            # TRUE
#'
#' # Comparing of data frames
#' d1 <- data.frame(A = v1, B = v2)
#' d2 <- data.frame(A = v2, B = v3)
#' d1 %lt% d1            # FALSE
#' d1 %lt% d2            # TRUE
#' d2 %lt% d1            # FALSE
#'
#' # Mixing it up
#' d1 %lt% NULL          # FALSE
#' v1 %lt% d1            # FALSE
#' 1 %lt% v1             # FALSE
#' @family operators
#' @export
`%lt%` <- function(x1, x2) {

  ret <- TRUE
  if (is.null(x1) & is.null(x2))
    ret <- FALSE
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
        if (any(!strong_lt(x1[[i]], x2[[i]]))) {
          ret <- FALSE
          break
        }
      }
    }

  } else {

    if (length(x1) != length(x2)) {
      ret <- FALSE
    } else {

      if (any(!strong_lt(x1, x2))) {
        ret <- FALSE
      }
    }
  }

  return(ret)
}

#' @noRd
strong_lt <- Vectorize(function(x1, x2) {

  ret <- TRUE
  if (is.null(x1) & is.null(x2))
    ret <- FALSE
  else if (is.null(x1) & !is.null(x2))
    ret <- FALSE
  else if (!is.null(x1) & is.null(x2))
    ret <- FALSE
  else if (is.na(x1) & is.na(x2))
    ret <- FALSE
  else if (is.na(x1) & !is.na(x2))
    ret <- FALSE
  else if (!is.na(x1) & is.na(x2))
    ret <- FALSE
  else {
    ret <- x1 < x2

  }

  return(ret)

})

#' @title Perform greater than comparison between two objects
#' @description The goal of the comparison operators is to return a TRUE
#' or FALSE value when any two objects are compared.  The operators provides a
#' simple, reliable equality check that allows comparing
#' of NULLs, NA values, and atomic data types without error. This operator
#' performs a greater than comparison.
#'
#' For data frames, the operator will compare all values in all columns, and return
#' a single TRUE if all values in the second data frame are greater than the
#' corresponding values in the first data frame.
#' @param x1 The first object to compare
#' @param x2 The second object to compare
#' @return A single TRUE or FALSE value indicating the results of the comparison.
#' @examples
#' # Comparing of NULLs and NA
#' NULL %gt% NULL        # FALSE
#' NULL %gt% NA          # FALSE
#' NA %gt% NA            # FALSE
#' 1 %gt% NULL           # FALSE
#' 1 %gt% NA             # FALSE
#'
#' # Comparing of atomic values
#' 1 %gt% 1              # FALSE
#' 2 %gt% 1              # TRUE
#' 1 %gt% 2              # FALSE
#' "one" %gt% "one"      # FALSE
#' 1 %gt% "one"          # FALSE
#' 1 %gt% Sys.Date()     # FALSE
#' Sys.Date() %gt% 1     # TRUE (Sys.Date() is a number)
#'
#' # Comparing of vectors
#' v1 <- c(0, 1, 2)
#' v2 <- c(1, 2, 3)
#' v3 <- c(2, 3, 4)
#' v1 %gt% v1            # FALSE
#' v1 %gt% v2            # FALSE
#' v2 %gt% v1            # TRUE
#' v3 %gt% v1            # TRUE
#'
#' # Comparing of data frames
#' d1 <- data.frame(A = v1, B = v2)
#' d2 <- data.frame(A = v2, B = v3)
#' d1 %gt% d1            # FALSE
#' d1 %gt% d2            # FALSE
#' d2 %gt% d1            # TRUE
#'
#' # Mixing it up
#' d1 %gt% NULL          # FALSE
#' v1 %gt% d1            # FALSE
#' 1 %gt% v1             # FALSE
#' @family operators
#' @export
`%gt%` <- function(x1, x2) {

  ret <- TRUE
  if (is.null(x1) & is.null(x2))
    ret <- FALSE
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
        if (any(!strong_gt(x1[[i]], x2[[i]]))) {
          ret <- FALSE
          break
        }
      }
    }

  } else {

    if (length(x1) != length(x2)) {
      ret <- FALSE
    } else {

      if (any(!strong_gt(x1, x2))) {
        ret <- FALSE
      }
    }
  }

  return(ret)
}

#' @noRd
strong_gt <- Vectorize(function(x1, x2) {

  ret <- TRUE
  if (is.null(x1) & is.null(x2))
    ret <- FALSE
  else if (is.null(x1) & !is.null(x2))
    ret <- FALSE
  else if (!is.null(x1) & is.null(x2))
    ret <- FALSE
  else if (is.na(x1) & is.na(x2))
    ret <- FALSE
  else if (is.na(x1) & !is.na(x2))
    ret <- FALSE
  else if (!is.na(x1) & is.na(x2))
    ret <- FALSE
  else {
    ret <- x1 > x2

  }

  return(ret)

})


