#' @title
#' Estimate the Individual Treatment Effect using Accelerated Bayesian Additive Regression Trees
#'
#' @description
#' Method for estimating the Individual Treatment Effect using Accelerated Bayesian Additive Regression Trees given a response vector, a treatment vector, and a features matrix
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param include_ps whether or not to include propensity score estimate as a covariate in ITE estimation
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
#' include_ps <- TRUE
#' ps_method <- "SL.xgboost"
#'
#' ite_list <- estimate_ite_xbart(y, z, X, include_ps, ps_method)
#'
estimate_ite_xbart <- function(y, z, X, include_ps, ps_method) {

  if (!requireNamespace("XBART", quietly = TRUE)) {
    stop(
      "Package \"XBART\" must be installed to use this function.",
      call. = FALSE
    )
  }


  if (include_ps) {
    est_ps <- estimate_ps(z, X, ps_method)
    X <- cbind(X, est_ps)
  }
  y_treated <- y[z==1]
  X_treated <- X[z==1,]
  y_control <- y[z==0]
  X_control <- X[z==0,]

  xbart_y1 <- XBART::XBART(y = as.matrix(y_treated), X = as.matrix(X_treated),
                           Xtest = as.matrix(X),num_trees = 30, num_sweeps = 1000,
                           max_depth = 250, Nmin = 1, num_cutpoints = 50,
                           alpha = 0.95, beta = 1.25, tau = stats::var(y_treated) / 30,
                           no_split_penality = "Auto", burnin = 100, mtry = ncol(X),
                           p_categorical = ncol(X), kap = 1, s = 1, verbose = FALSE,
                           parallel = TRUE)
  xbart_y0 <- XBART::XBART(y = as.matrix(y_control), X = as.matrix(X_control),
                           Xtest = as.matrix(X), num_trees = 30, num_sweeps = 1000,
                           max_depth = 250, Nmin = 1, num_cutpoints = 50,
                           alpha = 0.95, beta = 1.25, tau = stats::var(y_control) / 30,
                           no_split_penality = "Auto", burnin = 100, mtry = ncol(X),
                           p_categorical = ncol(X), kap = 1, s = 1, verbose = FALSE,
                           parallel = TRUE)

  pd_ite <- xbart_y1$yhats_test[, 100:1000] - xbart_y0$yhats_test[, 100:1000]
  ite <- apply(pd_ite, 1, mean)
  sd_ite <- apply(pd_ite, 1, stats::sd)
  return(list(ite, sd_ite))
}
