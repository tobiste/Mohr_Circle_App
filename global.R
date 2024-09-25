library(shiny)
library(dplyr)
library(ggplot2)
library(ggforce)

atand <- function(x) atan(x) * 180 / pi

acosd <- function(x) acos(x) * 180 / pi

sind <- function(x) sinpi(x / 180)

cosd <- function(x) cospi(x / 180)

deg2rad <- function(deg) deg * pi / 180

diff_stress <- function(s1, s3) {
  stopifnot(s1 >= s3)
  s1 - s3
}

mean_stress <- function(s1, s3) {
  stopifnot(s1 >= s3)
  (s1 + s3) / 2
}

shear_stress <- function(s1, s3, theta) {
  stopifnot(s1 >= s3)
  diff_stress(s1, s3) / 2 * sind(2 * theta)
}

normal_stress <- function(s1, s3, theta) {
  stopifnot(s1 >= s3)
  mean_stress(s1, s3) - diff_stress(s1, s3) / 2 * cosd(2 * theta)
}

theta <- function(mu) {
  (90 + atand(mu)) / 2
}

slip_tendency <- function(shear, normal) {
  shear / normal
}

dilatation_tendency <- function(s1, s3, normal) {
  (s1 - normal) / (s1 - s3)
}

#' Plot Mohr Circle in ggplots
#'
#' @param s1,s2,s3 numeric. magnitudes of sigma 1, 2, and 3, respectively.
#' @param coulomb numeric 2 element vector containing the coefficient of
#' cohesion and the coefficient of sliding friction for the Coulomb
#' criteria, e.g. (`c(70, 0.6)`)
#' @param sliding Sliding criteria (`0.81` by default)
#' @param units units of the `s1`, `s2`, and `s3` (`"MPa"` by default).
#'
#' @export
#' @import ggplot2
#' @importFrom ggforce geom_circle
#' @examples
#' ggMohr(1025, 400, 250)
ggMohr <- function(s1, s2, s3, coulomb = c(70, 0.6), sliding = 0.81, units = "MPa", fill = "gray", alpha = .5) {
  circle13.r <- diff_stress(s1, s3) / 2
  circle13.m <- mean_stress(s1, s3)

  circle12.r <- diff_stress(s1, s2) / 2
  circle12.m <- mean_stress(s1, s2)

  circle23.r <- diff_stress(s2, s3) / 2
  circle23.m <- mean_stress(s2, s3)

  if (!is.null(coulomb)) {
    theta.f <- theta(coulomb[2]) # (90 + atand(coulomb[2]))/2
  } else {
    theta.f <- 0
  }

  sigma_s <- shear_stress(s1, s3, theta.f / 2)
  sigma_n <- normal_stress(s1, s3, theta.f / 2)
  # theta.slope <- -atan(2*theta.f)

  ts <- slip_tendency(sigma_s, sigma_n)
  td <- dilatation_tendency(s1, s3, sigma_n)


  ggplot2::ggplot() +
    ggforce::geom_circle(aes(x0 = circle13.m, y0 = 0, r = circle13.r), fill = fill, alpha = alpha) +
    ggforce::geom_circle(aes(x0 = circle23.m, y0 = 0, r = circle23.r), fill = "white") +
    ggforce::geom_circle(aes(x0 = circle12.m, y0 = 0, r = circle12.r), fill = "white") +
    {
      if (!is.null(sliding)) ggplot2::geom_abline(intercept = 0, slope = sliding, lty = 2)
    } +
    {
      if (!is.null(coulomb)) ggplot2::geom_abline(intercept = coulomb[1], slope = coulomb[2], lty = 1)
    } +
    ggplot2::geom_point(aes(x = circle13.m, 0)) +
    ggplot2::geom_line(aes(x = c(circle13.m, sigma_n), y = c(0, sigma_s)), lty = 3) +
    ggplot2::geom_hline(yintercept = 0, alpha = .2) +
    ggplot2::geom_vline(xintercept = 0, alpha = .2) +
    ggplot2::geom_text(aes(x = (s1 + s3) / 2, y = 0), label = expression(sigma["m"]), vjust = -.5, hjust = -1) +
    ggplot2::geom_text(aes(x = s3, y = 0), label = expression(sigma[3]), vjust = -.5, hjust = -1) +
    ggplot2::geom_text(aes(x = s2, y = 0), label = expression(sigma[2]), vjust = -.5, hjust = -1) +
    ggplot2::geom_text(aes(x = s1, y = 0), label = expression(sigma[1]), vjust = -.5, hjust = -1) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      x = bquote(sigma[n] ~ (.(units))),
      y = bquote(sigma[s] ~ (.(units))),
      caption = bquote(
        theta["f"] == .(round(theta.f, 2)) ~ "|"
        ~ alpha["f"] == .(round(90 - theta.f, 2)) ~ "|"
        ~ "T"["s"] == .(round(ts, 1)) ~ "|"
        ~ "T"["d"] == .(round(td, 1))
      )
    ) +
    ggplot2::theme_classic()
}


ggMohr2 <- function(sigma_mean, sigma_diff, coulomb = c(70, 0.6), sliding = 0.81, units = "MPa", fill = "gray", alpha = .5) {
  s1 <- sigma_mean + sigma_diff / 2
  s3 <- sigma_mean - sigma_diff / 2


  if (!is.null(coulomb)) {
    theta.f <- theta(coulomb[2]) # (90 + atand(coulomb[2]))/2
  } else {
    theta.f <- 0
  }

  sigma_s <- shear_stress(s1, s3, theta.f / 2)
  sigma_n <- normal_stress(s1, s3, theta.f / 2)
  # theta.slope <- -atan(2*theta.f)

  ts <- slip_tendency(sigma_s, sigma_n)
  td <- dilatation_tendency(s1, s3, sigma_n)


  ggplot2::ggplot() +
    ggforce::geom_circle(aes(x0 = sigma_mean, y0 = 0, r = sigma_diff / 2), fill = fill, alpha = alpha) +
    # ggforce::geom_circle(aes(x0 = circle23.m, y0 = 0, r = circle23.r), fill = "white") +
    # ggforce::geom_circle(aes(x0 = circle12.m, y0 = 0, r = circle12.r), fill = "white") +
    {
      if (!is.null(sliding)) ggplot2::geom_abline(intercept = 0, slope = sliding, lty = 2)
    } +
    {
      if (!is.null(coulomb)) ggplot2::geom_abline(intercept = coulomb[1], slope = coulomb[2], lty = 1)
    } +
    ggplot2::geom_point(aes(x = sigma_mean, 0)) +
    ggplot2::geom_line(aes(x = c(sigma_mean, sigma_n), y = c(0, sigma_s)), lty = 3) +
    ggplot2::geom_hline(yintercept = 0, alpha = .2) +
    ggplot2::geom_vline(xintercept = 0, alpha = .2) +
    ggplot2::geom_text(aes(x = sigma_mean, y = 0), label = expression(sigma["m"]), vjust = -.5, hjust = -1) +
    ggplot2::geom_text(aes(x = s3, y = 0), label = expression(sigma[3]), vjust = -.5, hjust = -1) +
    # ggplot2::geom_text(aes(x = s2, y = 0), label = expression(sigma[2]), vjust = -.5, hjust = -1) +
    ggplot2::geom_text(aes(x = s1, y = 0), label = expression(sigma[1]), vjust = -.5, hjust = -1) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      x = bquote(sigma[n] ~ (.(units))),
      y = bquote(sigma[s] ~ (.(units))),
      caption = bquote(
        theta["f"] == .(round(theta.f, 2)) ~ "|"
        ~ alpha["f"] == .(round(90 - theta.f, 2)) ~ "|"
        ~ "T"["s"] == .(round(ts, 1)) ~ "|"
        ~ "T"["d"] == .(round(td, 1))
      )
    ) +
    ggplot2::theme_classic()
}
