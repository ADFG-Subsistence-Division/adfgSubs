combine_ci <- function(est, lower, upper, conf_level = 0.95) {
  est_name <- deparse(substitute(est))
  est_name <- paste0(toupper(substr(est_name, 1, 1)), substr(est_name, 2, nchar(est_name)))

  z <- qnorm(1 - (1 - conf_level)/2)
  se <- (upper - lower) / (2 * z)
  est_sum <- sum(est, na.rm = TRUE)
  se_sum <- sqrt(sum(se^2, na.rm = TRUE))
  lower_sum <- est_sum - z * se_sum
  upper_sum <- est_sum + z * se_sum

  # Return a tibble with uniquely named columns
  tibble(
    !!paste0("estimate", est_name) := est_sum,
    !!paste0("lower", est_name) := lower_sum,
    !!paste0("upper", est_name) := upper_sum
  )
}
