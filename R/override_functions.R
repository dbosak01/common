
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
#' @family overrides
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
#' top.  The default is TRUE. Parameter also accepts a vector of TRUE/FALSE
#' values, which correspond one to one with the number of by variables.
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
#' @family overrides
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

  # Set na.last if supplied
  if (length(na.last) < length(by)) {
    nlst <- rep(na.last, length(by))
  } else {
    nlst <- na.last
  }
  names(nlst) <- by


  # Create rank columns to handle custom sorts
  for (nm in by) {

    if (a[nm] == TRUE) {
      tmp[[nm]] <- rank(df[[nm]], na.last = nlst[nm])
    } else {
      tmp[[nm]] <- -rank(df[[nm]], na.last = !nlst[nm])
    }

    # Adjust rank for NA values.
    # If there are multiple NA values, R assigns an order
    # by the order encountered.  But they should
    # be assigned to top or bottom based on na.last.
    # Also need to consider ascending and descending.
    # Rank is negative for descending sort.
    if (any(is.na(df[[nm]]))) {
      if (nlst[nm] & a[nm]) {
        tmp[[nm]] <- ifelse(is.na(df[[nm]]), length(df[[nm]]), tmp[[nm]])
      } else if (!nlst[nm] & a[nm]) {
        tmp[[nm]] <- ifelse(is.na(df[[nm]]), 1, tmp[[nm]])
      } else if (nlst[nm] & !a[nm]) {
        tmp[[nm]] <- ifelse(is.na(df[[nm]]), -1, tmp[[nm]])
      } else if (!nlst[nm] & !a[nm]) {
        tmp[[nm]] <- ifelse(is.na(df[[nm]]), -length(df[[nm]]), tmp[[nm]])
      }
    }
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

    # Restore attributes
    ret <- copy.attributes(x, ret)
  }



  return(ret)


}
