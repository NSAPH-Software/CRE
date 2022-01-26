#' @title
#' Check Input Data
#'
#' @description
#' Conducts sanity checks for the input data
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the features matrix
#'
#' @return
#' Number of data samples.
#'
#'
check_input_data <- function(y, z, X){

  #---------------- Input data checks ------------------------------------------
  ## type
  if (!is.vector(y) & !is.numeric(y)){
    stop("Observed response vector (y) input values should be a numerical vector")
  }

  if (!is.vector(z) & !is.numeric(z)){
    stop("Treatment (z) input values should be a numerical vector.")
  }

  # if (!is.data.frame(X)){
  #   stop("Covariates (x) input values should be a data.frame.")
  # }

  ## size
  y_size <- length(y)
  z_size <- length(z)

  if (y_size != z_size){
    stop(paste("Response and and treatment vectors should be the same size. ",
               "Current values: ", y_size, ", ", z_size))
  }

  covars_size <- dim(X)

  if (covars_size[1] != y_size){
    stop(paste("Covariates (X) data.frame has different number of",
               "observation than response and treatment vectors.",
               "Current values: ", covars_size, ", ", y_size))
  }
  #-----------------------------------------------------------------------------

  invisible(y_size)
}
