#' @title
#' Check input data
#'
#' @description
#' Conducts sanity checks for the input data
#'
#' @param y The observed response vector.
#' @param z The treatment vector.
#' @param X The features matrix.
#' @param ite The estimated ITE vector.
#'
#' @keywords internal
#'
#' @return
#' The number of data samples.
#'
check_input_data <- function(y, z, X, ite = NULL) {

  logger::log_debug("Checking input data...")

  # Observed Outcome
  if (is.matrix(y)) {
    if (ncol(y) != 1 || !(is.numeric(y[, 1]) || is.integer(y[, 1]))) {
      stop("Observed response vector (y) input values should be a numerical
           vector, not a matrix")
    }
    N <- nrow(y)
  } else if (is.vector(y) && (is.numeric(y) || is.integer(y))) {
    N <- length(y)
  } else {
    stop("Observed response vector (y) input values should be a numerical
         vector.")
  }

  # Treatment
  if (is.matrix(z)) {
    if (ncol(z)!=1 || !(is.numeric(z[, 1]) || is.integer(z[, 1]))
        || length(unique(z)) != 2) {
      stop("Treatment vector (z) input values should be a numerical binary
           vector, not a matrix.")
    }
    N_check <- nrow(z)
  } else if (is.vector(z) && (is.numeric(z) || is.integer(z))
             && length(unique(z)) == 2) {
    N_check <- length(z)
  } else {
    stop(paste0("Treatment vector (z) input values should be",
                " a numerical binary vector."))
  }
  if (N != N_check) {
    stop(paste("Response and treatment vectors should be the same size.",
               " Current values:", N, ",", N_check))
  }

  # ITE (if provided)
  if (!is.null(ite)) {
    if (is.matrix(ite)) {
      if (ncol(ite) != 1 || !(is.numeric(ite[, 1]) || is.integer(ite[, 1]))) {
        stop("ITE vector (ite) input values should be a numerical
              vector, not a matrix")
      }
      N_check <- nrow(ite)
    } else if (is.vector(ite) && (is.numeric(ite) || is.integer(ite))) {
      N_check <- length(ite)
    } else {
      stop("ITE vector (ite) input values should be a numerical vector")
    }
    if (N != N_check) {
      stop(paste("Response and ITE vectors should be the same size.",
                 "Current values:", N, ",", N_check))
    }
  }

  # Covariates
  if (is.matrix(X)) {
    N_check <- nrow(X)
  } else if (is.data.frame(X)) {
    N_check <- nrow(X)
  } else {
    stop(paste("Invalid 'X' input. Please input a matrix or data frame",
               " of numeric variables"))
  }
  if (!all(apply(X, 2, class) %in% c("integer", "numeric"))) {
    stop(paste("Invalid 'X' input. Please input a matrix or data frame",
               " of numeric variables"))
  }
  if (N != N_check) {
    stop(paste("Response and X dataframe should have the ",
               "same number of observations.",
               "Current values:", N, ",", N_check))
  }

  logger::log_debug("Done with checking input data.")

  invisible(N)
}
