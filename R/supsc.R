

# Superscript -------------------------------------------------------------


#' @title
#' Converts a string to UTF-8 superscript
#' @encoding UTF-8
#' @description
#' The \code{supsc} function translates a normal character to a UTF-8
#' superscript character.  The function can be used to
#' generate superscripts for many common characters.  Most alphabetic
#' and numeric characters have UTF-8 superscripts.  This function is useful
#' because it saves you from having to look up the superscript character
#' code, or copy and pasting from the internet.  If a corresponding
#' superscript character code does not exist, a question mark
#' will be returned for that character.
#' @param x A string to be converted to superscript.
#' @return The superscript version of the string passed to the function,
#' if one exists. Otherwise, a question mark will be returned.
#' @seealso The \code{\link{subsc}} function to get subscripts.
#' @examples
#' # Single letter
#' paste0(supsc("a"), "Footnote")
#'
#' # Single number
#' paste0(supsc("1"), "Footnote")
#'
#' # Character string
#' paste0("December 5", supsc("th"))
#'
#' # Formula
#' paste0("x", supsc("(a+1)"))
#' @export
supsc <- function(x) {


  ret <- c()
  val <- c()

  lnms <- names(suplower)
  unms <- names(supupper)

  spl <- strsplit(x, split = "")

  for (item in seq_len(length(spl))) {

     vct <- spl[[item]]

     for (pos in seq_len(length(vct))) {


       if (vct[pos] %in% lnms)
         val[pos] <- suplower[vct[pos]]
       else if (vct[pos] %in% unms)
         val[pos] <- supupper[vct[pos]]
       else
         val[pos] <- supother(vct[pos])

     }

     ret[item] <- paste0(val, collapse = "")
  }

  return(ret)

}




# Utilities ---------------------------------------------------------------

supother <- function(oth) {

  ret <- NULL

  if (oth == 1)
    ret <- "\U00B9"
  else if (oth == 2)
    ret <- "\U00B2"
  else if (oth == 3)
    ret <- "\U00B3"
  else if (oth == 4)
    ret <- "\U2074"
  else if (oth == 5)
    ret <- "\U2075"
  else if (oth == 6)
    ret <- "\U2076"
  else if (oth == 7)
    ret <- "\U2077"
  else if (oth == 8)
    ret <- "\U2078"
  else if (oth == 9)
    ret <- "\U2079"
  else if (oth == 0)
    ret <- "\U2070"
  else if (oth == "i")
    ret <- "\U2071"
  else if (oth == "+")
    ret <- "\U207A"
  else if (oth == "-")
    ret <- "\U207B"
  else if (oth == "=")
    ret <- "\U207C"
  else if (oth == "(")
    ret <- "\U207D"
  else if (oth == ")")
    ret <- "\U207E"
  else if (oth == "n")
    ret <- "\U207F"
  else if (oth == "\U0153")
    ret <- "\UA7F9"
  else if (oth == "/")
    ret <- "\U141F"  #"\U2E0D"
  else if (oth == "*")
    ret <- "\U20F0"  #"⃰"
  else if (oth == " ")
    ret <- "\U2009"
  else
    ret <- "?"


  return(ret)
}


# Lookups -----------------------------------------------------------------


suplower <- c(a = "\U1D43",
              b = "\U1D47",
              c = "\U1D9C",
              d = "\U1D48",
              e = "\U1D49",
              f = "\U1DA0",
              g = "\U1D4D",
              h = "\U02B0",
              i = "\U2071",
              j = "\U02B2",
              k = "\U1D4F",
              l = "\U02E1",
              m = "\U1D50",
              n = "\U207F",
              o = "\U1D52",
              p = "\U1D56",
              q = "?",  # None
              r = "\U02B3",
              s = "\U02E2",
              t = "\U1D57",
              u = "\U1D58",
              v = "\U1D5B",
              w = "\U02B7",
              x = "\U02E3",
              y = "\U02B8",
              z = "\U1DBB")

supupper <- c(A = "\U1D2C",
              B = "\U1D2E",
              C = "\U1D04",
              D = "\U1D30",
              E = "\U1D31",
              'F' = "?",
              G = "\U1D33",
              H = "\U1D34",
              I = "\U1D35",
              J = "\U1D36",
              K = "\U1D37",
              L = "\U1D38",
              M = "\U1D39",
              N = "\U1D3A",
              O = "\U1D3C",
              P = "\U1D3E",
              Q = "?",  # None
              R = "\U1D3F",
              S = "?",  # None
              'T' = "\U1D40",
              U = "\U1D41",
              V = "\U2C7D",
              W = "\U1D42",
              X = "?",  # None
              Y = "?",  # None
              Z = "\U1D22")  # None


