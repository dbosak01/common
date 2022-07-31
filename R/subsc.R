
# Subscript -------------------------------------------------------------



subsc <- function(x) {

  ret <- c()
  val <- c()

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



subother <- function(oth) {

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
  else if (oth == 7)
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
  else
    ret <- oth


  return(ret)
}


# Lookups -----------------------------------------------------------------


sublower <- c(a = "\U1D43",
              b = "\U1D47",
              c = "\U1D9C",
              d = "\U1D48",
              e = "\U1D49",
              f = "\U1DA0",
              g = "",
              h = "",
              i = "",
              j = "",
              k = "",
              l = "",
              m = "",
              n = "",
              o = "",
              p = "",
              q = "",
              r = "",
              s = "",
              t = "",
              u = "",
              v = "",
              w = "",
              x = "",
              y = "",
              z = "")

subupper <- c(A = "",
              B = "",
              C = "",
              D = "",
              E = "",
              'F' = "",
              G = "",
              H = "",
              I = "",
              J = "",
              K = "",
              L = "",
              M = "",
              N = "",
              O = "",
              P = "",
              Q = "",
              R = "",
              S = "",
              'T' = "",
              U = "",
              V = "",
              W = "",
              X = "",
              Y = "",
              z = "")



