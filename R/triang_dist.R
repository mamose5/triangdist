#' Triangular Distribution Density
#' @param x vector of quantiles.
#' @param min lower limit.
#' @param max upper limit.
#' @param mode the mode.
#' @return A vector of densities.
#' @export
dtriang <- function(x, min, max, mode) {
  if (any(min >= max)) stop("min must be less than max")
  if (any(mode < min | mode > max)) stop("mode must be between min and max")

  ifelse(x < min | x > max, 0,
         ifelse(x < mode,
                2 * (x - min) / ((max - min) * (mode - min)),
                ifelse(x == mode,
                       2 / (max - min),
                       2 * (max - x) / ((max - min) * (max - mode)))))
}

#' Triangular Cumulative Distribution Function
#' @param q vector of quantiles.
#' @param min lower limit.
#' @param max upper limit.
#' @param mode the mode.
#' @return A vector of probabilities.
#' @export
ptriang <- function(q, min, max, mode) {
  if (any(min >= max)) stop("min must be less than max")
  if (any(mode < min | mode > max)) stop("mode must be between min and max")

  ifelse(q <= min, 0,
         ifelse(q >= max, 1,
                ifelse(q <= mode,
                       (q - min)^2 / ((max - min) * (mode - min)),
                       1 - (max - q)^2 / ((max - min) * (max - mode)))))
}

#' Triangular Quantile Function
#' @param p vector of probabilities.
#' @param min lower limit.
#' @param max upper limit.
#' @param mode the mode.
#' @return A vector of quantiles.
#' @export
qtriang <- function(p, min, max, mode) {
  if (any(min >= max)) stop("min must be less than max")
  if (any(mode < min | mode > max)) stop("mode must be between min and max")
  if (any(p < 0 | p > 1)) stop("p must be between 0 and 1")

  p_mode <- (mode - min) / (max - min)

  ifelse(p <= p_mode,
         min + sqrt(p * (max - min) * (mode - min)),
         max - sqrt((1 - p) * (max - min) * (max - mode)))
}

#' Triangular Random Generation
#' @param n number of observations.
#' @param min lower limit.
#' @param max upper limit.
#' @param mode the mode.
#' @return A vector of random numbers.
#' @importFrom stats runif
#' @export
rtriang <- function(n, min, max, mode) {
  u <- runif(n)
  qtriang(u, min, max, mode)
}
