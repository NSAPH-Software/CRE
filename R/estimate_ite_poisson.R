#' @title
#' Estimate the Individual Treatment Effect using Poisson Regression
#'
#' @description
#' Estimates the Individual Treatment Effect using Poisson Regression given a
#' response vector, a treatment vector, and a features matrix.
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#' @param X_names the names of the covariates
#' @param include_offset whether or not to include an offset when estimating
#' the ITE, for poisson only
#' @param offset_name the name of the offset, if it is to be included
#'
#' @return
#' a vector of ITE estimates
#'
#' @export
#'
#' @examples
#' dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#'                                 effect_size = 2, binary = FALSE)
#'
#' # Initialize parameters
#' y <- abs(dataset[["y"]])
#' z <- dataset[["z"]]
#' X <- as.data.frame(dataset[["X"]])
#' X_names <- names(as.data.frame(X))
#' include_offset <- FALSE
#' offset_name <- NA
#'
#' ite_list <- estimate_ite_poisson(y, z, X, X_names, include_offset, offset_name)
#'
estimate_ite_poisson <- function(y, z, X, X_names, include_offset, offset_name) {
  if (include_offset) {
    y_treated <- data.frame(y = y[z==1])
    X_treated <- as.data.frame(X[z==1, -which(X_names == offset_name)])
    y_control <- data.frame(y = y[z==0])
    X_control <- as.data.frame(X[z==0 ,-which(X_names == offset_name)])
    offset_treated <- data.frame(offset_treated = X[z==1,
                                                    which(X_names == offset_name)])
    offset_control <- data.frame(offset_control = X[z==0,
                                                    which(X_names == offset_name)])
    model_data_treated <- cbind(y_treated, X_treated)
    model_data_control <- cbind(y_control, X_control)
    temp1 <- stats::glm(y ~ ., data = model_data_treated,
                        offset = unlist(offset_treated),
                        family = stats::poisson(link = "log"))
    temp0 <- stats::glm(y ~ ., data = model_data_control,
                        offset = unlist(offset_control),
                        family = stats::poisson(link = "log"))
  } else {
    y_treated <- data.frame(y = y[z==1])
    X_treated <- as.data.frame(X[z==1,])
    y_control <- data.frame(y = y[z==0])
    X_control <- as.data.frame(X[z==0,])
    model_data_treated <- cbind(y_treated, X_treated)
    model_data_control <- cbind(y_control, X_control)
    temp1 <- stats::glm(y ~ ., data = model_data_treated,
                        family = stats::poisson(link = "log"))
    temp0 <- stats::glm(y ~ ., data = model_data_control,
                        family = stats::poisson(link = "log"))
  }
  y1hat <- suppressWarnings(stats::predict(temp1, as.data.frame(X)))
  y0hat <- suppressWarnings(stats::predict(temp0, as.data.frame(X)))
  ite <- y1hat - y0hat
  return(ite)
}
