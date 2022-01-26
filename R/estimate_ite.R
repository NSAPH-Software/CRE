#' @title
#' Estimate the Individual Treatment Effect
#'
#' @description
#' Estimates the Individual Treatment Effect given a response vector,
#' a treatment vector, a features matrix, and a desired algorithm.
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param ite_method the method for estimating the Individual Treatment Effect:
#'   - `ipw`: Inverse Propensity Weighting
#'   - `sipw`: Stabilized Inverse Propensity Weighting
#'   - `aipw`: Augmented Inverse Propensity Weighting
#'   - `or`: Outcome Regression, TODO: change this into a non reserved term.
#'   - `bart`: BART
#'   - `xbart`: Accelerated BART
#'   - `bcf`: Bayesian Causal Forest
#'   - `xbcf`: Accelerated Bayesian Causal Forest
#'   - `cf`: Causal Forest
#'   - `poisson`: Poisson Estimation
#' @param include_ps whether or not to include propensity score estimate as a
#' covariate in ITE estimation
#' @param ps_method estimation method for the propensity score
#' @param or_method the estimation model for the outcome regressions in estimate_ite_aipw
#' @param binary whether or not the outcome is binary
#' @param X_names the names of the covariates
#' @param include_offset whether or not to include an offset when estimating
#' the ITE, for poisson only
#' @param offset_name the name of the offset, if it is to be included
#'
#' @return
#' a list that includes:
#'   -  raw ITE estimates
#'   -  standardized ITE estimates, and
#'   -  standard deviations for the ITE estimates.
#'
#' @export
#'
#' @examples
#' dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#'                                 effect_size = 2, binary = FALSE)
#'
#' # Initialize parameters
#' y <- dataset[["y"]]
#' z <- dataset[["z"]]
#' X <- as.data.frame(dataset[["X"]])
#' ite_method <- "bart"
#' include_ps <- TRUE
#' ps_method <- "SL.xgboost"
#' or_method <- NA
#' binary <- FALSE
#' X_names <- names(as.data.frame(X))
#' include_offset <- FALSE
#' offset_name <- NA
#'
#' ite_list <- estimate_ite(y, z, X, ite_method, include_ps, ps_method, or_method,
#'                          binary, X_names, include_offset, offset_name)
#'
estimate_ite <- function(y, z, X, ite_method, include_ps, ps_method, or_method,
                         binary, X_names, include_offset, offset_name) {

  if (ite_method == "ipw") {
    ite <- estimate_ite_ipw(y, z, X, ps_method)
    sd_ite <- NA
  } else if (ite_method == "sipw") {
    ite <- estimate_ite_sipw(y, z, X, ps_method)
    sd_ite <- NA
  } else if (ite_method == "aipw") {
    ite <- estimate_ite_aipw(y, z, X, ps_method, or_method)
    sd_ite <- NA
  } else if (ite_method == "or") {
    ite <- estimate_ite_or(y, z, X)
    sd_ite <- NA
  } else if (ite_method == "bart") {
    ite_results <- estimate_ite_bart(y, z, X, include_ps, ps_method)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "xbart") {
    ite_results <- estimate_ite_xbart(y, z, X, include_ps, ps_method)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "bcf") {
    ite_results <- estimate_ite_bcf(y, z, X, ps_method)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "xbcf") {
    ite_results <- estimate_ite_xbcf(y, z, X, ps_method)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "cf") {
    ite_results <- estimate_ite_cf(y, z, X, include_ps, ps_method)
    ite <- ite_results[[1]]
    sd_ite <- ite_results[[2]]
  } else if (ite_method == "poisson") {
    ite <- estimate_ite_poisson(y, z, X, X_names, include_offset, offset_name)
    sd_ite <- NA
  } else {
    stop(paste("Invalid ITE method. Please choose from the following:\n",
               "'ipw', 'sipw', 'aipw', 'or', 'bart', 'xbart', 'bcf', 'xbcf', ",
               "'cf', or 'poisson'"))
  }
  if (binary) {
    ite <- round(ite, 0)
  }
  ite_std <- (ite - mean(ite)) / stats::sd(ite)
  return(list(ite = as.vector(ite), ite_std = as.vector(ite_std),
              sd_ite = as.vector(sd_ite)))
}
