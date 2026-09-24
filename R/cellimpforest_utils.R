# Internal helpers for cellImpForest(). Pure functions, tested in
# inst/tinytest/test_cellimpforest_utils.R and test_cellimpforest_engines.R.

#' Tukey bisquare weights for standardised residuals (NA stays NA)
#' @noRd
.cif_bisquare <- function(z, psi_c = 4.685) {
  w <- (1 - (z / psi_c)^2)^2
  w[!is.na(z) & abs(z) > psi_c] <- 0
  w[is.na(z)] <- NA_real_
  w
}

#' Two-sided normal tail probability of a standardised residual (small = surprising)
#' @noRd
.cif_surprise <- function(z) 2 * stats::pnorm(-abs(z))

#' Robust global residual scale of one column: MAD, falling back to SD, then to 1
#' @param r residuals, NA where the cell is missing or flagged
#' @param use logical, cells allowed into the scale (observed, unflagged, training rows)
#' @noRd
.cif_scale <- function(r, use) {
  s <- stats::mad(r[use], na.rm = TRUE)
  if (!is.finite(s) || s <= 0) s <- stats::sd(r[use], na.rm = TRUE)
  if (!is.finite(s) || s <= 0) s <- 1
  s
}

#' Relative cross-fitted probability of the observed level of a categorical cell
#' @noRd
.cif_rho <- function(prob, y) {
  idx <- match(as.character(y), colnames(prob))
  p_obs <- prob[cbind(seq_len(nrow(prob)), idx)]
  p_max <- apply(prob, 1, function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE))
  rho <- p_obs / p_max
  rho[!is.finite(rho)] <- NA_real_
  rho
}

#' Most frequent level (ties: first), NA ignored
#' @noRd
.cif_mode <- function(v) {
  tab <- table(v, useNA = "no")
  names(tab)[which.max(tab)]
}
