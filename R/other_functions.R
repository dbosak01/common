


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



#' @title Search for names
#' @description A function to search for variable names in a data.frame or tibble.
#' The function features wild card pattern matching, start and end
#' boundaries, and names to exclude.
#' @param x A data frame or tibble whose names to search.  Parameter also
#' accepts a character vector of names.
#' @param pattern A vector of patterns to search for. The asterisk (*)
#' and question mark (?) characters may be used to indicate partial matches.
#' @param exclude A vector of patterns to exclude from the search results.
#' The asterisk (*)
#' and question mark (?) characters may be used to indicate partial matches.
#' @param start A variable name or position to start the search. Default is 1.
#' @param end A variable name or position to end the search. Default is the
#' length of the name vector.
#' @param ignore.case Whether to perform a case sensitive or insensitive
#' search.  Valid values are TRUE and FALSE. Default is TRUE.
#' @returns A vector of variable names that met the search criteria.
#' @examples
#' # Show all names for reference
#' names(mtcars)
#' #  [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear" "carb"
#'
#' # Names that start with "c"
#' find.names(mtcars, "c*")
#' # [1] "cyl"  "carb"
#'
#' # Names that start with "c" or "d"
#' find.names(mtcars, c("c*", "d*"))
#' # [1] "cyl"  "carb" "disp" "drat"
#'
#' # Names between "disp" and "qsec"
#' find.names(mtcars, start = "disp", end = "qsec")
#' # [1] "disp" "hp"   "drat" "wt"   "qsec"
#'
#' # Names that start with "c" or "d" after position 5
#' find.names(mtcars, c("c*", "d*"), start = 5)
#' # [1] "carb" "drat"
#'
#' # Names between "disp" and "qsec" excluding "wt"
#' find.names(mtcars, start = "disp", end = "qsec", exclude = "wt")
#' # [1] "disp" "hp"   "drat" "qsec"
#' @export
find.names <- function(x, pattern = NULL, exclude = NULL,
                       start = NULL, end = NULL, ignore.case = TRUE) {

  if ("data.frame" %in% class(x))
    ret <- names(x)
  else if ("character" %in% class(x))
    ret <- x
  else
    stop("Input parameter x must be a data.frame or character vector")

  sp <- 1
  ep <- length(ret)


  # Assign start if passed
  if (!is.null(start)) {
    if (is.character(start)) {
      sp <- match(start, ret, nomatch = 1)
    } else {

      sp <- start
    }
  }

  # Assign end if passed
  if (!is.null(end)) {
    if (is.character(end)) {
      ep <- match(end, ret, nomatch = length(ret))
    } else {

      ep <- end
    }
  }

  # Subset start and end
  ret <- ret[seq(sp, ep)]

  if (!is.null(ret) & !is.null(pattern)) {

    srch <- glob2rx(pattern)

    tmp2 <- c()
    for (sr in srch) {
      tmp1 <- ret[grepl(sr, ret, ignore.case = ignore.case)]
      tmp2 <- append(tmp2, tmp1)
    }
    ret <- tmp2

  }

  if (!is.null(ret) & !is.null(exclude)) {

    excl <- glob2rx(exclude)

    for (ex in excl) {
      ret <- ret[!grepl(ex, ret, ignore.case = ignore.case)]
    }

  }

  if (length(ret) == 0)
    ret <- NULL

  return(ret)
}


#' @title Copy attributes between two data frames
#' @description A function to copy column attributes from one
#' data frame to another.  The function will copy all attributes attached
#' to each column.  The column order does not matter, and the data frames
#' do not need identical structures. The matching occurs by column name,
#' not position.  Any existing attributes on the target data frame
#' that do not match the source data frame will be retained unaltered.
#' @param source A data frame to copy attributes from.
#' @param target A data frame to copy attributes to.
#' @returns The data frame in the \code{target} parameter, with updated
#' attributes from \code{source}.
#' @family overrides
#' @examples
#' # Prepare data
#' dat1 <- mtcars
#' dat2 <- mtcars
#'
#' # Set labels for dat1
#' labels(dat1) <- list(mpg = "Miles Per Gallon",
#'                      cyl = "Cylinders",
#'                      disp = "Displacement")
#'
#' # Copy labels from dat1 to dat2
#' dat2 <- copy.attributes(dat1, dat2)
#'
#' # View results
#' labels(dat2)
#' # $mpg
#' # [1] "Miles Per Gallon"
#' #
#' # $cyl
#' # [1] "Cylinders"
#' #
#' # $disp
#' # [1] "Displacement"
#' @export
copy.attributes <- function(source, target) {

  if (is.null(source)) {

    stop("Parameter 'source' cannot be null.")
  }


  if (is.null(target)) {

    stop("Parameter 'target' cannot be null.")
  }


  if (!"data.frame" %in% class(source)) {

    stop("Object 'source' must be a data.frame.")

  }

  if (!"data.frame" %in% class(target)) {

    stop("Object 'target' must be a data.frame.")

  }

  if (!ncol(source) > 0) {
    stop("Object 'source' must have at least one column.")

  }

  if (!ncol(target) > 0) {
    stop("Object 'target' must have at least one column.")

  }


  ret <- target

  for (nm in names(target)) {

    att <- attributes(source[[nm]])
    if (!is.null(att)) {

      for (at in names(att)) {

        attr(ret[[nm]], at) <- att[[at]]

      }
    }
  }

  return(ret)
}

