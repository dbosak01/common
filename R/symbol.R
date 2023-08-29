

# Symbols -----------------------------------------------------------------



#' @title
#' Gets UTF-8 symbol characters
#' @encoding UTF-8
#' @description
#' The \code{symbol} function gets UTF-8 symbol characters. You
#' can call this function to look up trademarks, Greek letters,
#' and mathematical operators.  The function uses HTML entity
#' keywords to indicate which symbol to return.  You may pass more than
#' one keyword in a single call to get a combined result.  Any characters not
#' recognized as a keyword will be left alone.  Characters surrounded by
#' square brackets ([]) will be subscripted.  Characters surrounded by square
#' brackets and prefixed with an up arrow (^[]) will be superscripted.
#' @section Keywords:
#' The following symbol keywords are available:
#' \itemize{
#'   \item \strong{Trademark and Copyright}: copy, reg, trade
#'   \item \strong{Financial}: cent, euro, pound, rupee, ruble, yen, yuan
#'   \item \strong{Mathmatical}: asymp, bcong, cong, coprod, empty, fnof,
#'    ge, int, Int, infin, le, ncong, ne, not, part, plusmn,
#'    prod, radic, sime, sum
#'   \item \strong{Logical}: and, cap, cup, comp, cuvee, cuwed, exist,
#'   forall, fork, isin, nexist, ni, notin,
#'   notni, nsub, nsup, or, sub, sup, xcap, xcup, xvee, xwedge
#'   \item \strong{Greek uppercase letters}: Alpha, Beta, Gamma, Delta, Epsilon,
#'   Zeta, Eta, Theta, Iota, Kappa, Lambda, Mu, Nu, Xi, Omicron, Pi, Rho,
#'   Sigma, Tau, Upsilon, Phi, Chi, Psi, Omega
#'   \item \strong{Greek lowercase letters}: alpha, beta, gamma, delta, epsilon,
#'   zeta, eta, theta, iota, kappa, lambda, mu, nu, xi, omicron, pi, rho,
#'   sigma, tau, upsilon, phi, chi, psi, omega
#'   \item \strong{Arrows}: rarr, larr, barr, uarr, darr, harr, rArr, lArr, uArr, dArr, hArr
#'   \item \strong{Other Symbols}: dagger, ddagger, deg, permil, pertenk, sect
#'   }
#' @param keyword A symbol keyword. This keyword follows HTML conventions. See
#' the \strong{Keywords} section for a complete list of all supported keywords.
#' @return The requested UTF-8 symbol character or characters.
#' @family utf8
#' @examples
#' # Trademark symbol
#' symbol("My Companytrade")
#'
#' # Registered Trademark symbol
#' symbol("My Companyreg")
#'
#' # Dagger symbol concatenated
#' paste0(symbol("dagger"), "My footnotes")
#'
#' # Alpha squared
#' symbol("alpha^[2]")
#'
#' # Greek Symbols
#' symbol("SigmaPsiZeta")
#'
#' # Useful Math Symbols
#' symbol("asymp ge le ne plusmn empty fnof radic sum")
#'
#' # Useful Logical Symbols
#' symbol("forall isin notin cup cap and or")
#'
#' # Chemistry
#' symbol("2H[2] + O[2] barr 2H[2]O")
#' @export
symbol <- function(keyword) {


  ret <- keyword

  # Superscripts
  res <- regexec("\\^\\[.+?\\]", ret)
  while (res[[1]] > 0) {
    tmp1 <- substr(ret, res[[1]], res[[1]] + attr(res[[1]], "match.length") - 1)
    tmp2 <- substr(ret, res[[1]] + 2, res[[1]] + attr(res[[1]], "match.length") - 2)
    ret <- gsub(tmp1, supsc(tmp2), ret, fixed = TRUE)
    res <- regexec("\\^\\[.+?\\]", ret)
  }

  # Subscripts
  res <- regexec("\\[.+?\\]", ret)
  while (res[[1]] > 0) {
    tmp1 <- substr(ret, res[[1]], res[[1]] + attr(res[[1]], "match.length") - 1)
    tmp2 <- substr(ret, res[[1]] + 1, res[[1]] + attr(res[[1]], "match.length") - 2)
    ret <- gsub(tmp1, subsc(tmp2), ret, fixed = TRUE)
    res <- regexec("\\[.+?\\]", ret)
  }

  # Trademark and Copyright
  ret <- gsub("trade", "\U2122", ret, fixed = TRUE)
  ret <- gsub("reg", "\U00AE", ret, fixed = TRUE)
  ret <- gsub("copy", "\U00A9", ret, fixed = TRUE)


  # Finance
  ret <- gsub("cent", "\U00A2", ret, fixed = TRUE)
  ret <- gsub("pound", "\U00A3", ret, fixed = TRUE)
  ret <- gsub("yen", "\U00A5", ret, fixed = TRUE)
  ret <- gsub("yuan", "CN\U00A5", ret, fixed = TRUE)
  ret <- gsub("euro", "\U20AC", ret, fixed = TRUE)
  ret <- gsub("rupee", "\U20B9", ret, fixed = TRUE)
  ret <- gsub("ruble", "\U20BD", ret, fixed = TRUE)


  # Logical =  forall, comp, exist, nexist, isin, notin, ni, fork
  # notni, cap, cup, sub, sup, nsub, nsup, xwedge, xvee, xcap, xcup, cuvee, cuwed,
  ret <- gsub("forall", "\U2200", ret, fixed = TRUE)
  ret <- gsub("comp", "\U2201", ret, fixed = TRUE)
  ret <- gsub("nexist", "\U2204", ret, fixed = TRUE)
  ret <- gsub("exist", "\U2203", ret, fixed = TRUE)
  ret <- gsub("isin", "\U2208", ret, fixed = TRUE)
  ret <- gsub("notin", "\U2209", ret, fixed = TRUE)
  ret <- gsub("fork", "\U22D4", ret, fixed = TRUE)
  ret <- gsub("notni", "\U220C", ret, fixed = TRUE)
  ret <- gsub("nsub", "\U2284", ret, fixed = TRUE)
  ret <- gsub("nsup", "\U2285", ret, fixed = TRUE)
  ret <- gsub("xwedge", "\U22C0", ret, fixed = TRUE)
  ret <- gsub("xvee", "\U22C1", ret, fixed = TRUE)
  ret <- gsub("xcap", "\U22C2", ret, fixed = TRUE)
  ret <- gsub("xcup", "\U22C3", ret, fixed = TRUE)
  ret <- gsub("cuvee", "\U22CE", ret, fixed = TRUE)
  ret <- gsub("cuwed", "\U22CF", ret, fixed = TRUE)
  ret <- gsub("cap", "\U2229", ret, fixed = TRUE)
  ret <- gsub("cup", "\U222A", ret, fixed = TRUE)
  ret <- gsub("sub", "\U2282", ret, fixed = TRUE)
  ret <- gsub("sup", "\U2283", ret, fixed = TRUE)
  ret <- gsub("ni", "\U220B", ret, fixed = TRUE)

  # Greek Uppercase
  ret <- gsub("Alpha", "\U0391", ret, fixed = TRUE)
  ret <- gsub("Beta", "\U0392", ret, fixed = TRUE)
  ret <- gsub("Gamma", "\U0393", ret, fixed = TRUE)
  ret <- gsub("Delta", "\U0394", ret, fixed = TRUE)
  ret <- gsub("Epsilon", "\U0395", ret, fixed = TRUE)
  ret <- gsub("Zeta", "\U0396", ret, fixed = TRUE)
  ret <- gsub("Eta", "\U0397", ret, fixed = TRUE)
  ret <- gsub("Theta", "\U0398", ret, fixed = TRUE)
  ret <- gsub("Iota", "\U0399", ret, fixed = TRUE)
  ret <- gsub("Kappa", "\U039A", ret, fixed = TRUE)
  ret <- gsub("Lambda", "\U039B", ret, fixed = TRUE)
  ret <- gsub("Mu", "\U039C", ret, fixed = TRUE)
  ret <- gsub("Nu", "\U039D", ret, fixed = TRUE)
  ret <- gsub("Xi", "\U039E", ret, fixed = TRUE)
  ret <- gsub("Omicron", "\U039F", ret, fixed = TRUE)
  ret <- gsub("Pi", "\U03A0", ret, fixed = TRUE)
  ret <- gsub("Rho", "\U03A1", ret, fixed = TRUE)
  ret <- gsub("Sigma", "\U03A3", ret, fixed = TRUE)
  ret <- gsub("Tau", "\U03A4", ret, fixed = TRUE)
  ret <- gsub("Upsilon", "\U03A5", ret, fixed = TRUE)
  ret <- gsub("Phi", "\U03A6", ret, fixed = TRUE)
  ret <- gsub("Chi", "\U03A7", ret, fixed = TRUE)
  ret <- gsub("Psi", "\U03A8", ret, fixed = TRUE)
  ret <- gsub("Omega", "\U03A9", ret, fixed = TRUE)

  # Greek Lowercase
  ret <- gsub("alpha", "\U03B1", ret, fixed = TRUE)
  ret <- gsub("beta", "\U03B2", ret, fixed = TRUE)
  ret <- gsub("gamma", "\U03B3", ret, fixed = TRUE)
  ret <- gsub("delta", "\U03B4", ret, fixed = TRUE)
  ret <- gsub("epsilon", "\U03B5", ret, fixed = TRUE)
  ret <- gsub("zeta", "\U03B6", ret, fixed = TRUE)
  ret <- gsub("theta", "\U03B8", ret, fixed = TRUE)
  ret <- gsub("eta", "\U03B7", ret, fixed = TRUE)  # search last
  ret <- gsub("iota", "\U03B9", ret, fixed = TRUE)
  ret <- gsub("kappa", "\U03BA", ret, fixed = TRUE)
  ret <- gsub("lambda", "\U03BB", ret, fixed = TRUE)
  ret <- gsub("mu", "\U03BC", ret, fixed = TRUE)
  ret <- gsub("nu", "\U03BD", ret, fixed = TRUE)
  ret <- gsub("xi", "\U03BE", ret, fixed = TRUE)
  ret <- gsub("omicron", "\U03BF", ret, fixed = TRUE)
  ret <- gsub("pi", "\U03C0", ret, fixed = TRUE)
  ret <- gsub("rho", "\U03C1", ret, fixed = TRUE)
  ret <- gsub("sigmaf", "\U03C2", ret, fixed = TRUE)
  ret <- gsub("sigma", "\U03C3", ret, fixed = TRUE)
  ret <- gsub("tau", "\U03C4", ret, fixed = TRUE)
  ret <- gsub("upsilon", "\U03C5", ret, fixed = TRUE)
  ret <- gsub("phi", "\U03C6", ret, fixed = TRUE)
  ret <- gsub("chi", "\U03C7", ret, fixed = TRUE)
  ret <- gsub("psi", "\U03C8", ret, fixed = TRUE)
  ret <- gsub("omega", "\U03C9", ret, fixed = TRUE)

  # Other - dagger, ddagger, sect, deg, permil, pertenk
  ret <- gsub("ddagger", "\U2021", ret, fixed = TRUE)
  ret <- gsub("dagger", "\U2020", ret, fixed = TRUE)
  ret <- gsub("sect", "\U00A7", ret, fixed = TRUE)
  ret <- gsub("deg", "\U00B0", ret, fixed = TRUE)
  ret <- gsub("permil", "\U2030", ret, fixed = TRUE)
  ret <- gsub("pertenk", "\U2031", ret, fixed = TRUE)

  # Math - asymp, bcong, cong, coprod, empty, fnof,
  #    ge, int, Int, le, ncong, ne, not, part, plusmn,
  #    prod, radic, sime, sum
  ret <- gsub("asymp", "\U2248", ret, fixed = TRUE)
  ret <- gsub("bcong", "\U224C", ret, fixed = TRUE)
  ret <- gsub("ncong", "\U2247", ret, fixed = TRUE)
  ret <- gsub("cong", "\U2245", ret, fixed = TRUE)
  ret <- gsub("coprod", "\U2210", ret, fixed = TRUE)
  ret <- gsub("empty", "\U2205", ret, fixed = TRUE)
  ret <- gsub("fnof", "\U0192", ret, fixed = TRUE)
  ret <- gsub("ge", "\U2265", ret, fixed = TRUE)
  ret <- gsub("int", "\U222B", ret, fixed = TRUE)
  ret <- gsub("Int", "\U222C", ret, fixed = TRUE)
  ret <- gsub("infin", "\U221E", ret, fixed = TRUE)
  ret <- gsub("le", "\U2264", ret, fixed = TRUE)
  ret <- gsub("ne", "\U2260", ret, fixed = TRUE)
  ret <- gsub("not", "\U00AC", ret, fixed = TRUE)
  ret <- gsub("part", "\U2202", ret, fixed = TRUE)
  ret <- gsub("plusmn", "\U00B1", ret, fixed = TRUE)
  ret <- gsub("prod", "\U220F", ret, fixed = TRUE)
  ret <- gsub("radic", "\U221A", ret, fixed = TRUE)
  ret <- gsub("sime", "\U2243", ret, fixed = TRUE)
  ret <- gsub("sum", "\U2211", ret, fixed = TRUE)

  # Arrows - rarr, larr, barr, uarr, darr, harr, rArr, lArr,
  # uArr, dArr, hArr
  ret <- gsub("rarr", "\U2192", ret, fixed = TRUE)
  ret <- gsub("larr", "\U2190", ret, fixed = TRUE)
  ret <- gsub("barr", "\U21C6", ret, fixed = TRUE)
  ret <- gsub("uarr", "\U2191", ret, fixed = TRUE)
  ret <- gsub("darr", "\U2193", ret, fixed = TRUE)
  ret <- gsub("harr", "\U2194", ret, fixed = TRUE)
  ret <- gsub("lArr", "\U21D0", ret, fixed = TRUE)
  ret <- gsub("uArr", "\U21D1", ret, fixed = TRUE)
  ret <- gsub("rArr", "\U21D2", ret, fixed = TRUE)
  ret <- gsub("dArr", "\U21D3", ret, fixed = TRUE)
  ret <- gsub("hArr", "\U21D4", ret, fixed = TRUE)

  # More logical
  ret <- gsub("and", "\U2227", ret, fixed = TRUE)
  ret <- gsub("or", "\U2228", ret, fixed = TRUE)

  return(ret)

}

