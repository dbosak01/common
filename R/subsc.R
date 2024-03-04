
# Subscript -------------------------------------------------------------


#' @title
#' Converts a string to UTF-8 subscript
#' @encoding UTF-8
#' @description
#' The \code{subsc} function translates a normal character to a UTF-8
#' subscript character.  The function can be used to
#' generate subscripts for many common characters.  All
#' numeric characters and some lower case letters have UTF-8 subscripts.
#' There are no upper case subscript letters.
#' This function is useful
#' because it saves you from having to look up the subscript character
#' code, or copy and paste from the internet.  If a corresponding
#' subscript character code does not exist, a question mark
#' will be returned for that character.
#' @param x A string to be converted to subscript.
#' @return The subscript version of the string passed to the function,
#' if one exists. Otherwise, a question mark will be returned.
#' @family utf8
#' @examples
#' # Date string
#' paste0("December 5", subsc("th"))
#'
#' # Chemistry
#' paste0("H", subsc("2"), "SO", subsc("4"))
#' @export
subsc <- function(x) {

  ret <- c()
  val <- c()

  if (!is.character(x)) {
    x <- as.character(x)
  }

  lnms <- names(sublower)
  unms <- names(subupper)

  spl <- strsplit(x, split = "")

  for (item in seq_len(length(spl))) {

    vct <- spl[[item]]

    for (pos in seq_len(length(vct))) {


      if (vct[pos] %in% lnms)
        val[pos] <- sublower[vct[pos]]
      else if (vct[pos] %in% unms)
        val[pos] <- subupper[vct[pos]]
      else
        val[pos] <- subother(vct[pos])

    }

    ret[item] <- paste0(val, collapse = "")
  }

  return(ret)




}




# Lookups -----------------------------------------------------------------


combother <- function(comb) {

  ret <- comb

  if (comb == "1/2")
    ret <- "\U00BD"
  else


  return(ret)
}


subother <- function(oth) {

  ret <- NULL

  if (oth == 1)
    ret <- "\U2081"
  else if (oth == 2)
    ret <- "\U2082"
  else if (oth == 3)
    ret <- "\U2083"
  else if (oth == 4)
    ret <- "\U2084"
  else if (oth == 5)
    ret <- "\U2085"
  else if (oth == 6)
    ret <- "\U2086"
  else if (oth == 7)
    ret <- "\U2087"
  else if (oth == 8)
    ret <- "\U2088"
  else if (oth == 9)
    ret <- "\U2089"
  else if (oth == 0)
    ret <- "\U2080"
  else if (oth == "+")
    ret <- "\U208A"
  else if (oth == "-")
    ret <- "\U208B"
  else if (oth == "=")
    ret <- "\U208C"
  else if (oth == "(")
    ret <- "\U208D"
  else if (oth == ")")
    ret <- "\U208E"
  else if (oth == "/")
    ret <- "\U2E1D"
  else
    ret <- oth


  return(ret)
}


# Lookups -----------------------------------------------------------------


sublower <- c(a = "\U2090",
              b = "?",
              c = "?",
              d = "?",
              e = "\U2091",
              f = "?",
              g = "?",
              h = "\U2095",
              i = "?",
              j = "?",
              k = "\U2096",
              l = "\U2097",
              m = "\U2098",
              n = "\U2099",
              o = "\U2092",
              p = "\U209A",
              q = "?",
              r = "?",
              s = "\U209B",
              t = "\U209C",
              u = "?",
              v = "?",
              w = "?",
              x = "\U2093",
              y = "?",
              z = "?")

subupper <- c(A = "?",
              B = "?",
              C = "?",
              D = "?",
              E = "?",
              'F' = "?",
              G = "?",
              H = "?",
              I = "?",
              J = "?",
              K = "?",
              L = "?",
              M = "?",
              N = "?",
              O = "?",
              P = "?",
              Q = "?",
              R = "?",
              S = "?",
              'T' = "?",
              U = "?",
              V = "?",
              W = "?",
              X = "?",
              Y = "?",
              Z = "?")



