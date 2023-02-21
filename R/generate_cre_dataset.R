#' @title
#' Generate CRE synthetic data
#'
#' @description
#' Generates synthetic data with continues or binary outcome.
#'
#' @param n An integer number that represents the number of observations.
#' Non-integer values will be converted into an integer number.
#' @param rho A positive double number that represents the correlation
#' within the covariates (default: 0, range: (0,1)).
#' @param n_rules The number of causal rules. (default: 2, range: {1,2,3,4}).
#' @param effect_size The effect size magnitude in (default: 2, range: >=0).
#' @param p The number of covariates (default: 10).
#' @param binary_covariates Whether to use binary or continuous covariates
#' (default: TRUE).
#' @param binary_outcome Whether to use binary or continuous outcomes
#' (default: TRUE).
#' @param confounding Only for continuous outcome, add confounding variables:
#' - Linear confounding "lin".
#' - Non-linear confounding "nonlin".
#' - No confounding "no" (default).
#'
#' @return
#' A list of synthetic data containing:
#' - An outcome vector (`y`),
#' - A treatment vector (`z`),
#' - A covariates matrix (`X`) and
#' - An individual treatment vector (`ite`)
#'
#' @note
#' Set (binary/continuous) covariates domain (`binary_covariates`).
#' Set (binary/continuous) outcome domain (`binary_outcome`).
#' Increase complexity in heterogeneity discovery:
#' - Decreasing the sample size (`n`),
#' - adding correlation among variables (`rho`),
#' - increasing the number of rules (`n_rules`),
#' - increasing the number of covariates (`p`),
#' - decreasing the absolute value of the causal effect (`effect_size`),
#' - adding linear or not-linear confounders (`confounding`).
#'
#' @examples
#' set.seed(123)
#' dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#'                                 effect_size = 2, binary_covariates = TRUE,
#'                                 binary_outcome = TRUE, confounding = "no")
#'
#' @export
#'
generate_cre_dataset <- function(n = 1000, rho = 0, n_rules = 2, p = 10,
                                 effect_size = 2, binary_covariates = TRUE,
                                 binary_outcome = TRUE, confounding = "no") {

  # Check for correct binary input
  if (!(binary_outcome %in% c(TRUE, FALSE))) {
    stop("Invalid 'binary' input. Please specify TRUE or FALSE.")
  }
  if (is.numeric(n) && !is.integer(n)) {
    n <- as.integer(n)
  }

  # Generate Covariate Matrix
  mu <- rep(0, p)
  Sigma <- matrix(rho, nrow = p, ncol = p) + diag(p) * (1 - rho)
  rawvars <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
  pvars <- stats::pnorm(rawvars)
  if (binary_covariates) {
    X <- stats::qbinom(pvars, 1, 0.5)
  } else {
    X <- pvars
  }
  colnames(X) <- paste("x", 1:p, sep = "")
  X <- as.data.frame(X)

  # Generate Treatment Vector
  logit_prob <- -1 + X$x1 - X$x2 + X$x3
  prob <- exp(logit_prob) / (1 + exp(logit_prob))
  z <- stats::rbinom(n, 1, prob = prob)

  # Generate Causal Rules and Potential Outcomes
  if (binary_outcome == TRUE) {
    y0 <- rep(0, n)
    y1 <- rep(0, n)
    effect_size <- 1
  } else {
    if (confounding == "lin") {
      mean <- X$x1 + X$x3 + X$x4
    } else if (confounding == "nonlin") {
      mean <- X$x1 + cos(X$x3)
    } else if (confounding == "no") {
      mean <- 0
    } else {
      stop("Invalid 'confounding' input. Please input:
           'lin' for linear confounding,
           'nonlin' for non-linear confounding),
           'no' for no confounding")
    }
    y0 <- stats::rnorm(n, mean = mean, sd = 1)
    y1 <- y0
  }
  if (n_rules >= 1) {
    y0[X$x1 > 0.5 & X$x2 <= 0.5] <- y0[X$x1 > 0.5 & X$x2 <= 0.5] + effect_size
  }
  if (n_rules >= 2) {
    y1[X$x5 > 0.5 & X$x6 <= 0.5] <- y1[X$x5 > 0.5 & X$x6 <= 0.5] + effect_size
  }
  if (n_rules >= 3) {
    if (binary_outcome) {
      stop(paste("Synthtic dataset with binary outcome and ", n_rules,
                 "rules has not been implemented yet. ",
                 "Available 'n_rules' options: {1,2}."))
    }
    y0[X$x4 > 0.5] <- y0[X$x4 > 0.5] + 0.5 * effect_size
  }
  if (n_rules >= 4) {
    y1[X$x5 <= 0.5 & X$x7 > 0.5 & X$x8 <= 0.5] <-
                y1[X$x5 <= 0.5 & X$x7 > 0.5 & X$x8 <= 0.5] + 2 * effect_size
  }
  if (n_rules >= 5) {
    stop(paste("Synthtic dataset with continuos outcome and ", n_rules,
               "rules has not been implemented yet. ",
               "Available 'n_rules' options: {1,2,3,4}."))
  }


  # Generate Outcome
  y <- y0 * (1 - z) + y1 * z

  # Compute Individual Treatment Effect
  ite <- y1 - y0

  # Generate Observed Data
  dataset <- list(y = y, z = z, X = X, ite = ite)
  return(dataset)
}
