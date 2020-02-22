# Functions from other packages to reduce dependancies
# stplanr::od_id_szudzik

#' The Szudzik pairing function, on two vectors of equal
#' length. It returns a vector of ID numbers.
#'
#' This function superseeds od_id_order as it is faster on large datasets
#' @param x a vector of numeric, character, or factor values
#' @param y a vector of numeric, character, or factor values
#' @param ordermatters logical, does the order of values matter to pairing, default = FALSE
#' @noRd
#'
od_id_szudzik <- function(x, y, ordermatters = FALSE) {
  if (length(x) != length(y)) {
    stop("x and y are not of equal length")
  }

  if (class(x) == "factor") {
    x <- as.character(x)
  }
  if (class(y) == "factor") {
    y <- as.character(y)
  }
  lvls <- unique(c(x, y))
  x <- as.integer(factor(x, levels = lvls))
  y <- as.integer(factor(y, levels = lvls))
  if (ordermatters) {
    ismax <- x > y
    stplanr.key <- (ismax * 1) * (x^2 + x + y) + ((!ismax) * 1) * (y^2 + x)
  } else {
    a <- ifelse(x > y, y, x)
    b <- ifelse(x > y, x, y)
    stplanr.key <- b^2 + a
  }
  return(stplanr.key)
}
