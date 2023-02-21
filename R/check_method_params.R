#' @title
#' Check method-related parameters
#'
#' @description
#' Checks method-related parameters.
#'
#' @param y The observed response vector.
#' @param ite The estimated ITE vector.
#' @param params The list of parameters required to run the method functions.
#'
#' @keywords internal
#'
#' @return
#' A modified input `params`. A list of parameters that might be changed during
#' the checks.
#'
check_method_params <- function(y, ite, params) {

  logger::log_debug("Checking method parameters...")

  # Honest Splitting Parameters Check ------------------------------------------
  ratio_dis <- getElement(params, "ratio_dis")
  if (length(ratio_dis) == 0) {
    ratio_dis <- 0.5
  } else {
    if (!inherits(ratio_dis, "numeric") || (ratio_dis < 0) || (ratio_dis > 1)) {
      stop("Invalid 'ratio_dis' input. Please input a number between 0 and 1.")
    }
  }
  params[["ratio_dis"]] <- ratio_dis


  # ITE Estimation Parameters Check --------------------------------------------
  ite_method_dis <- tolower(getElement(params, "ite_method_dis"))
  if (length(ite_method_dis) == 0) {
    ite_method_dis <- "aipw"
  } else {
    if (!(ite_method_dis %in% c("aipw", "slearner", "tlearner", "xlearner",
                                "bart", "bcf", "cf", "tpoisson"))) {
      stop(paste(
        "Invalid ITE method for Discovery Subsample. Please choose ",
        "from the following:\n", "'aipw', 'bart', 'slearner','tlearner', ",
        "'xlearner', 'bcf', 'cf', or 'tpoisson'"
      ))
    }
  }
  params[["ite_method_dis"]] <- ite_method_dis

  ite_method_inf <- tolower(getElement(params, "ite_method_inf"))
  if (length(ite_method_inf) == 0) {
    ite_method_inf <- "aipw"
  } else {
    if (!(ite_method_dis %in% c("aipw", "slearner", "tlearner", "xlearner",
                                "bart", "bcf", "cf", "tpoisson"))) {
      stop(paste(
        "Invalid ITE method for Inference Subsample. Please choose ",
        "from the following:\n", "'aipw', 'bart', 'slearner','tlearner', ",
        "'xlearner', 'bcf', 'cf', or 'tpoisson'"
      ))
    }
  }
  params[["ite_method_inf"]] <- ite_method_inf


  # Propensity Score Estimation Parameters Check--------------------------------

  ps_method_dis <- getElement(params, "ps_method_dis")
  if (!(ite_method_dis %in% c("slearner", "tlearner",
                              "xlearner", "tpoisson"))) {
    if (length(ps_method_dis) == 0) {
      ps_method_dis <- "SL.xgboost"
    } else {
      if (!(class(ps_method_dis) %in% c("character", "list"))) {
        stop("Please specify a string or list of strings for the ps_method_dis
           argument.")
      }
    }
  } else {
    ps_method_dis <- NA
  }
  params[["ps_method_dis"]] <- ps_method_dis

  ps_method_inf <- getElement(params, "ps_method_inf")
  if (!(ite_method_inf %in% c("slearner", "tlearner",
                              "xlearner", "tpoisson"))) {
    if (length(ps_method_inf) == 0) {
      ps_method_inf <- "SL.xgboost"
    } else {
      if (!(class(ps_method_inf) %in% c("character", "list"))) {
        stop("Please specify a string or list of strings for the ps_method_inf
           argument.")
      }
    }
  } else {
    ps_method_inf <- NA
  }
  params[["ps_method_inf"]] <- ps_method_inf

  # Outcome Regression Score Estimation Parameters Check -----------------------
  oreg_method_dis <- getElement(params, "oreg_method_dis")
  if (ite_method_dis %in% c("slearner", "tlearner", "xlearner", "aipw")) {
    if (length(oreg_method_dis) == 0) {
      oreg_method_dis <- "SL.xgboost"
    } else {
      if (!(class(oreg_method_dis) %in% c("character", "list"))) {
        stop("Please specify a string or list of strings for the oreg_method_dis
           argument.")
      }
    }
  } else {
    oreg_method_dis <- NA
  }
  params[["oreg_method_dis"]] <- oreg_method_dis

  oreg_method_inf <- getElement(params, "oreg_method_inf")
  if (ite_method_inf %in% c("slearner", "tlearner", "xlearner", "aipw")) {
    if (length(oreg_method_inf) == 0) {
      oreg_method_inf <- "SL.xgboost"
    } else {
      if (!(class(oreg_method_inf) %in% c("character", "list"))) {
        stop("Please specify a string or list of strings for the oreg_method_inf
           argument.")
      }
    }
  } else {
    oreg_method_inf <- NA
  }
  params[["oreg_method_inf"]] <- oreg_method_inf


  # Discard ITE Parameters if ITE estimates are provided------------------------
  if (!is.null(ite)) {
    params[["ite_method_dis"]] <- "personalized"
    params[["ps_method_dis"]] <- NULL
    params[["or_method_dis"]] <- NULL
    params[["ite_method_inf"]] <- "personalized"
    params[["ps_method_inf"]] <- NULL
    params[["or_method_inf"]] <- NULL
  }

  logger::log_debug("Done with checking method parameters.")

  return(params)
}
