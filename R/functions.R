

# Overrides ---------------------------------------------------------------



#' @title
#' Get or set labels for a data frame
#'
#' @description
#' The \code{labels} function extracts all assigned labels from a
#' data frame, and returns them in a named list. The function also
#' assigns labels from a named list.  This function is a data frame-specific
#' implementation of the Base R \code{\link[base]{labels}} function.
#'
#' @details
#' If labels are assigned to the "label" attributes of the data frame
#' columns, the \code{labels} function will extract those labels.  The
#' function will return the labels in a named list, where the names
#' correspond to the name of the column that the label was assigned to.
#' If a column does not have a label attribute assigned, that column
#' will not be included in the list.
#'
#' When used on the receiving side of an assignment, the function will assign
#' labels to a data frame.  The labels should be in a named list, where
#' each name corresponds to the data frame column to assign the label to.
#'
#' Finally, if you wish to clear out the label attributes, assign
#' a NULL value to the \code{labels} function.
#' @param object A data frame or tibble.
#' @param ... Follow-on parameters.  Required for generic function.
#' @return A named list of labels. The labels must be quoted strings.
#' @aliases labels<-
#' @examples
#' # Take subset of data
#' df1 <- mtcars[1:10, c("mpg", "cyl")]
#'
#' # Assign labels
#' labels(df1) <- list(mpg = "Mile Per Gallon", cyl = "Cylinders")
#'
#' # Examine attributes
#' str(df1)
#' # 'data.frame':	10 obs. of  2 variables:
#' # $ mpg: num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2
#' # ..- attr(*, "label")= chr "Mile Per Gallon"
#' # $ cyl: num  6 6 4 6 8 6 8 4 4 6
#' # ..- attr(*, "label")= chr "Cylinders"
#'
#' # View assigned labels
#' labels(df1)
#' # $mpg
#' # [1] "Mile Per Gallon"
#' #
#' # $cyl
#' # [1] "Cylinders"
#'
#' # Clear labels
#' labels(df1) <- NULL
#'
#' # Display Cleared Labels
#' labels(df1)
#' # list()
#' @export
labels.data.frame <- function(object, ...) {


  if (!"data.frame" %in% class(object))
    stop("Object class list must contain 'data.frame'.")

  ret <- list()

  for (nm in names(object)) {

    if (!is.null(attr(object[[nm]], "label", exact = TRUE))) {
      ret[[nm]] <- attr(object[[nm]], "label", exact = TRUE)
    }

  }

  return(ret)

}


#' @aliases labels.data.frame
#' @rdname  labels.data.frame
#' @param x A data frame or tibble
#' @param value A named list of labels. The labels must be quoted strings.
#' @export
`labels<-` <- function(x, value) {

  if (!"data.frame" %in% class(x))
    stop("Class list must contain 'data.frame'.")



  if (all(is.null(value))) {

    for (nm in names(x)) {

      attr(x[[nm]], "label") <- NULL
    }


  } else {

    for (nm in names(value)) {

      if (!is.null(x[[nm]]))
        attr(x[[nm]], "label") <- value[[nm]]

    }

  }

  return(x)

}


#' @title Sorts a data frame
#' @description An overload to the Base R \code{\link[base]{sort}} function for
#' data frames.  Allows multiple columns to be sorted easily.  Also
#' allows you to control the sort direction for each column independently.
#' @param x The input data frame to sort.
#' @param decreasing This parameter was added to conform to the S3 generic
#' method signature of the \code{\link{sort}} function, and will be
#' ignored here.  Please use the \code{ascending} parameter.
#' @param ... This parameter is required for the generic method signature.
#' Anything passed on it will be ignored.
#' @param by A vector of column names to sort by.  If this parameter
#' is not supplied, the function will sort by all columns in order
#' from left to right.
#' @param ascending A vector of TRUE or FALSE values corresponding
#' to the variables on the \code{by} parameter.  These values will determine
#' the direction to sort each column. Ascending is TRUE, and descending is FALSE.
#' The vector will be recycled if it is short, and truncated if it is long.
#' By default, all variables will be sorted ascending.
#' @param na.last Whether to put NA values first or last in the sort. If TRUE,
#' NA values will sort to the bottom.  If FALSE, NA values will sort to the
#' top.  The default is TRUE.
#' @param index.return Whether to return the sorted data frame or a vector
#' of sorted index values.  If this parameter is TRUE, the function
#' will return sorted index values.  By default, the parameter is FALSE,
#' and will return the sorted data frame.
#' @return The function returns either a sorted data frame or a
#' sorted vector of row index values, depending on the value of the
#' \code{index.return} parameter.  If \code{index.return} is FALSE,
#' the function will return the sorted data frame.
#' If the \code{index.return} parameter is TRUE, it will return a vector
#' of row indices.
#' @examples
#' # Prepare unsorted sample data
#' dt <- mtcars[1:10, 1:3]
#' dt
#' #                    mpg cyl  disp
#' # Mazda RX4         21.0   6 160.0
#' # Mazda RX4 Wag     21.0   6 160.0
#' # Datsun 710        22.8   4 108.0
#' # Hornet 4 Drive    21.4   6 258.0
#' # Hornet Sportabout 18.7   8 360.0
#' # Valiant           18.1   6 225.0
#' # Duster 360        14.3   8 360.0
#' # Merc 240D         24.4   4 146.7
#' # Merc 230          22.8   4 140.8
#' # Merc 280          19.2   6 167.6
#'
#' # Sort by mpg ascending
#' dt1 <- sort(dt, by = "mpg")
#' dt1
#' #                    mpg cyl  disp
#' # Duster 360        14.3   8 360.0
#' # Valiant           18.1   6 225.0
#' # Hornet Sportabout 18.7   8 360.0
#' # Merc 280          19.2   6 167.6
#' # Mazda RX4         21.0   6 160.0
#' # Mazda RX4 Wag     21.0   6 160.0
#' # Hornet 4 Drive    21.4   6 258.0
#' # Datsun 710        22.8   4 108.0
#' # Merc 230          22.8   4 140.8
#' # Merc 240D         24.4   4 146.7
#'
#' # Sort by mpg descending
#' dt1 <- sort(dt, by = "mpg", ascending = FALSE)
#' dt1
#' #                    mpg cyl  disp
#' # Merc 240D         24.4   4 146.7
#' # Datsun 710        22.8   4 108.0
#' # Merc 230          22.8   4 140.8
#' # Hornet 4 Drive    21.4   6 258.0
#' # Mazda RX4         21.0   6 160.0
#' # Mazda RX4 Wag     21.0   6 160.0
#' # Merc 280          19.2   6 167.6
#' # Hornet Sportabout 18.7   8 360.0
#' # Valiant           18.1   6 225.0
#' # Duster 360        14.3   8 360.0
#'
#' # Sort by cyl then mpg
#' dt1 <- sort(dt, by = c("cyl", "mpg"))
#' dt1
#' #                    mpg cyl  disp
#' # Datsun 710        22.8   4 108.0
#' # Merc 230          22.8   4 140.8
#' # Merc 240D         24.4   4 146.7
#' # Valiant           18.1   6 225.0
#' # Merc 280          19.2   6 167.6
#' # Mazda RX4         21.0   6 160.0
#' # Mazda RX4 Wag     21.0   6 160.0
#' # Hornet 4 Drive    21.4   6 258.0
#' # Duster 360        14.3   8 360.0
#' # Hornet Sportabout 18.7   8 360.0
#'
#' # Sort by cyl descending then mpg ascending
#' dt1 <- sort(dt, by = c("cyl", "mpg"),
#'             ascending = c(FALSE, TRUE))
#' dt1
#' #                    mpg cyl  disp
#' # Duster 360        14.3   8 360.0
#' # Hornet Sportabout 18.7   8 360.0
#' # Valiant           18.1   6 225.0
#' # Merc 280          19.2   6 167.6
#' # Mazda RX4         21.0   6 160.0
#' # Mazda RX4 Wag     21.0   6 160.0
#' # Hornet 4 Drive    21.4   6 258.0
#' # Datsun 710        22.8   4 108.0
#' # Merc 230          22.8   4 140.8
#' # Merc 240D         24.4   4 146.7
#' @export
sort.data.frame <- function(x, decreasing = FALSE, ..., by = NULL,
                            ascending = TRUE, na.last = TRUE,
                            index.return = FALSE) {

  if (!"data.frame" %in% class(x)) {
    stop("Input object must be derived from a data frame")

  }

  nms <- names(x)
  if (!all(by %in% nms)) {
    lst <- by[!by %in% nms]

    stop(paste0("By value '", lst, "' is not a column on the input data frame."))

  }

  # A temporary list to hold columns
  tmp <- list()

  # Store input dataset in new variable
  df <- x

  # Default by is all variable names
  if (is.null(by))
    by <- names(df)

  # Default ascending
  a <- rep(TRUE, length(by))

  # Set ascending if supplied
  if (!is.null(ascending)) {
    a <- rep(ascending, length(by))
  }
  names(a) <- by

  # Create rank columns to handle custom sorts
  for (nm in by) {

    if (a[nm] == TRUE)
      tmp[[nm]] <- rank(df[[nm]], na.last = na.last)
    else
      tmp[[nm]] <- -rank(df[[nm]], na.last = na.last)
  }

  # Get modified dataframe
  tmp <- as.data.frame(tmp, stringsAsFactors = FALSE)

  # Get row order
  ord <- do.call('order', tmp)

  if (index.return) {
    ret <- ord
  } else {
    # Sort input dataframe
    ret <- df[ord, , drop = FALSE]
  }


  return(ret)


}


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



# File Operations ---------------------------------------------------------


#' @title Returns the path of the current program
#' @description A function that gets the full path of the currently running
#' program.  If the function fails to retrieve the path for some reason,
#' it will return a NULL.  The function takes no parameters.
#'
#' Credit for this function goes to Andrew Simmons and the
#' \code{\link{this.path}} package.
#' @returns The full path of the currently running program, or a NULL.
#' @examples
#' # Get current path
#' pth <- Sys.path()
#' pth
#' # [1] "C:/programs/myprogram.R"
#' @export
Sys.path <- function() {

  ppth <- NULL
  tryCatch({
    ppth <- this.path::this.path()
  }, error = function(e) { ppth <- NULL})

  return(ppth)

}

#' @title Search for files
#' @description A function to find files on the file system. The function
#' starts from the directory specified in the \code{path} parameter,
#' and searches outward in a radiating pattern
#' for the file name in the \code{pattern} parameter.
#' Results are returned as a vector of full paths in the order encountered.
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




# Other Functions ---------------------------------------------------------




#' @title Rounds numbers up
#' @description A function that rounds positive numbers up when the last digit
#' is a 5.  For negative numbers ending in 5, the function actually rounds down.
#' "Round away from zero" is the most accurate description of this function.
#' @param x A vector of values to round.
#' @param digits A number of decimal places to round to. Default is zero.
#' @returns The rounded data vector.
#' @examples
#' # Round to even
#' round(2.4)   # 2
#' round(2.5)   # 2
#' round(-2.5)  # -2
#' round(2.6)   # 3
#'
#' # Round up
#' roundup(2.4)  # 2
#' roundup(2.5)  # 3
#' roundup(-2.5) # -3
#' roundup(2.6)  # 3
#'
#' @export
roundup <- function(x, digits = 0) {

  if (!is.numeric(x)) {
    stop("Input value must be numeric.")
  }

  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  ret <- z*posneg

  return(ret)
}



#' @title Combine unquoted values
#' @description A function to quote and combine unquoted values.
#' The function will return a vector of quoted values.  This function
#' allows you to use non-standard evaluation for any parameter
#' that accepts a string or vector of strings.
#' @param ... One or more unquoted values.
#' @returns A vector of quoted values.
#' @examples
#' # Combine unquoted values
#' v(var1, var2, var3)
#' # [1] "var1" "var2" "var3"
#'
#' # Data frame subset
#' dat <- mtcars[1:5, v(mpg, cyl, disp)]
#' dat
#' #                    mpg cyl disp
#' # Mazda RX4         21.0   6  160
#' # Mazda RX4 Wag     21.0   6  160
#' # Datsun 710        22.8   4  108
#' # Hornet 4 Drive    21.4   6  258
#' # Hornet Sportabout 18.7   8  360
#'
#' # Data frame sort
#' dat2 <- sort(dat, by = v(cyl, mpg))
#' dat2
#' #                    mpg cyl disp
#' # Datsun 710        22.8   4  108
#' # Mazda RX4         21.0   6  160
#' # Mazda RX4 Wag     21.0   6  160
#' # Hornet 4 Drive    21.4   6  258
#' # Hornet Sportabout 18.7   8  360
#' @export
v <- function(...) {

  # Determine if it is a vector or not.  "language" is a vector.
  # if (typeof(substitute(..., env = environment())) == "language")
  #   vars <- substitute(..., env = environment())
  # else
    vars <- substitute(list(...), env = environment())

  # Turn each item into a character
  vars_c <- c()
  if (length(vars) > 1) {
    for (i in 2:length(vars)) {
      vars_c[[length(vars_c) + 1]] <- paste0(deparse(vars[[i]]), combine = "")
    }

  }

  # Convert list to vector
  vars_c <- unlist(vars_c)


  return(vars_c)

}



