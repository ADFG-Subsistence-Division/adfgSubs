#' gini_mad
#'
#' Calculates the Gini coefficient of a series of values using the mean absolute difference method.
#'
#' @param y A vector of ranked harvest values.
#'
#' @returns The Gini coefficient.
#'
#' @details
#' Citation: Cowell, F. A. 2011. Measuring Inequality. 3rd edition. Oxford University Press. Gini coefficient formula on page 155.
#'
#' @export
#'

gini_mad <- function(y) {
  n <- length(y)

  (1/(2*(n^2)*mean(y))) * sum(outer(y, y, FUN = function(a,b) abs(a-b)))
}


#' gini_lorenz
#'
#' Calculates the Gini coefficient of a series of values using the Lorenz curve area method.
#'
#' @param x A vector of values.
#'
#' @details Citation: Sen, A. 1997. On Economic Inequality. Clarendon Press.
#' @returns The Gini coefficient.
#' @export
#'

gini_lorenz <- function(x) {
  # Step 1: Sort values
  x <- sort(x)
  n <- length(x)

  # Step 2: Compute cumulative proportions
  cum_x <- cumsum(x) / sum(x)  # Cumulative harvest share
  cum_pop <- (1:n) / n         # Cumulative population share

  # Step 3: Compute area under Lorenz curve using trapezoidal rule
  lorenz_area <- sum((cum_x[-1] + cum_x[-n]) * diff(cum_pop)) / 2

  # Step 4: Compute Gini coefficient
  gini <- 1 - 2 * lorenz_area

  return(gini)

}
