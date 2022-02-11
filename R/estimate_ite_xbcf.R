#' @title
#' Estimate the Individual Treatment Effect using Accelerated Bayesian Causal Forest
#'
#' @description
#' Method for estimating the Individual Treatment Effect using Accelerated Bayesian Causal Forest given a response vector, a treatment vector, and a features matrix
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param ps_method estimation method for the propensity score
#'
#' @return a list of ITE estimates and standard deviations for the ITE estimates
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
#' ps_method <- "SL.xgboost"
#'
#' if (requireNamespace("XBCF", quietly = TRUE)) {
#'     ite_list <- estimate_ite_xbcf(y, z, X, ps_method)
#' }
#'
estimate_ite_xbcf <- function(y, z, X, ps_method) {

  if (!requireNamespace("XBCF", quietly = TRUE)) {
    stop(
      "Package \"XBCF\" must be installed to use this function.",
      call. = FALSE
    )
  }


  est_ps <- estimate_ps(z, X, ps_method)
  xbcf_model <- XBCF::XBCF(y = as.matrix(y), z = as.matrix(z), x_con = as.matrix(X),
                           x_mod = as.matrix(X), pihat = as.matrix(est_ps),
                           mtry_con = ncol(X), mtry_mod = ncol(X),
                           pcat_con = ncol(X), pcat_mod = ncol(X),
                           n_trees_con = 200, tau_con = 10 * stats::var(y) / 200,
                           n_trees_mod = 50, tau_mod = 1 * stats::var(y) / 50)
  pd_ite_temp <- xbcf_model$tauhats * stats::sd(y)
  b_xbcf <- xbcf_model$b_draws
  for (i in 16:40) {
    pd_ite_temp[, i] <- pd_ite_temp[, i] * (b_xbcf[i, 2] - b_xbcf[i, 1])
  }
  pd_ite <- pd_ite_temp[, 16:40]
  ite <- apply(pd_ite, 1, mean)
  sd_ite <- apply(pd_ite, 1, stats::sd)
  return(list(ite, sd_ite))
}
