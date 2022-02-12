#' @title
#' The Causal Rule Ensemble
#'
#' @description
#' Performs the Causal Rule Ensemble on a dataset with a response variable,
#'  a treatment variable, and various features
#'
#' @param y the observed response vector
#' @param z the treatment vector
#' @param X the covariate matrix
#' @param ratio_dis the ratio of data delegated to the discovery sub-sample
#' @param ite_method_dis the method to estimate the discovery sample ITE
#' @param include_ps_dis whether or not to include propensity score estimate
#'  as a covariate in discovery ITE estimation, considered only for BART, XBART,
#'   or CF
#' @param ps_method_dis the estimation model for the propensity score on the
#'   discovery subsample
#' @param or_method_dis the estimation model for the outcome regressions in
#'   estimate_ite_aipw on the discovery subsample
#' @param ite_method_inf the method to estimate the inference sample ITE
#' @param include_ps_inf whether or not to include propensity score estimate as
#'  a covariate in inference ITE estimation, considered only for BART, XBART,
#'   or CF
#' @param ps_method_inf the estimation model for the propensity score on the
#'   inference subsample
#' @param or_method_inf the estimation model for the outcome regressions in
#'   estimate_ite_aipw on the inference subsample
#' @param ntrees_rf the number of decision trees for randomForest
#' @param ntrees_gbm the number of decision trees for gradient boosting
#' @param min_nodes the minimum size of the trees' terminal nodes
#' @param max_nodes the maximum size of the trees' terminal nodes
#' @param t the common support used in generating the causal rules matrix
#' @param q the selection threshold used in selecting the causal rules
#' @param stability_selection whether or not using stability selection for selecting the causal rules
#' @param pfer_val the Per-Family Error Rate, the expected number of false discoveries
#' @param include_offset whether or not to include an offset when estimating
#'  the ITE, for Poisson only
#' @param offset_name the name of the offset, if it is to be included
#' @param cate_method the method to estimate the CATE values
#' @param cate_SL_library the library used if cate_method is set to DRLearner
#' @param filter_cate whether or not to filter rules with p-value <= 0.05
#'
#' @return
#' an S3 object containing the matrix of Conditional
#'  Average Treatment Effect estimates
#'
#' @export
#'
#' @examples
#' dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
#'                                 effect_size = 2, binary = FALSE)
#'
#' cre_results <- cre(y = dataset[["y"]], z = dataset[["z"]],
#'                    X = as.data.frame(dataset[["X"]]), ratio_dis = 0.25,
#'                    ite_method_dis = "bart", include_ps_dis = TRUE,
#'                    ite_method_inf = "bart", include_ps_inf = TRUE,
#'                    ntrees_rf = 100, ntrees_gbm = 50, min_nodes = 20,
#'                    max_nodes = 5, t = 0.025, q = 0.8)
#'
cre <- function(y, z, X, ratio_dis, ite_method_dis, include_ps_dis = NA,
                ps_method_dis = "SL.xgboost", or_method_dis = NA,
                ite_method_inf, include_ps_inf = NA,
                ps_method_inf = "SL.xgboost", or_method_inf = NA,
                ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q,
                stability_selection = TRUE, pfer_val = 0.1,
                include_offset = FALSE, offset_name = NA,
                cate_method = "DRLearner", cate_SL_library = "SL.xgboost",
                filter_cate = FALSE) {

  # Input checks ---------------------------------------------------------------
  if (!(class(y) %in% c("numeric", "integer"))){
    stop("Invalid 'y' input. Please input a numeric vector.")
  }

  if (length(unique(z)) != 2){
    stop("Invalid 'z' input. Please input a binary treatment vector.")
  }

  if (length(class(X)) == 1) {
    if (!(class(X) %in% c("data.frame", "matrix"))) {
      stop("Invalid 'X' input. Please input a matrix or data frame.")
    }
  }

  if (length(class(X)) == 2) {
    if (!(identical(class(X), c("matrix", "array")))) {
      stop("Invalid 'X' input. Please input a matrix or data frame.")
    }
  }

  X_classes <- apply(X, 2, class)

  if (!all(X_classes %in% c("integer", "numeric"))){
    stop(paste("Invalid 'X' input. Please input a matrix or data frame",
               " of numeric categorical variables"))
  }

  # todo: dplyr between is not necessary.
  if (class(ratio_dis) != "numeric" | !dplyr::between(ratio_dis, 0, 1)){
    stop("Invalid 'ratio_dis' input. Please input a number between 0 and 1.")
  }

  if (class(ntrees_rf) != "numeric"){
    stop("Invalid 'ntrees_rf' input. Please input a number.")
  }

  if (class(ntrees_gbm) != "numeric"){
    stop("Invalid 'ntrees_gbm' input. Please input a number.")
  }

  if (class(min_nodes) != "numeric"){
    stop("Invalid 'min_nodes' input. Please input a number.")
  }

  if (class(max_nodes) != "numeric"){
    stop("Invalid 'max_nodes' input. Please input a number.")
  }

  if (class(t) != "numeric"){
    stop("Invalid 't' input. Please input a number.")
  }

  if (class(q) != "numeric"){
    stop("Invalid 'q' input. Please input a number.")
  }

  if (!(class(cate_SL_library) %in% c("character", "logical"))){
    stop("Invalid 'q' input. Please input a number.")
  }

  ite_method_dis <- tolower(ite_method_dis)
  if (!(ite_method_dis %in% c("ipw", "sipw", "aipw", "or", "bart", "xbart",
                              "bcf", "xbcf", "cf", "poisson"))) {
    stop(paste("Invalid ITE method for Discovery Subsample. Please choose ",
               "from the following:\n","'ipw', 'sipw', 'aipw', or', 'bart', ",
               "'xbart', 'bcf', 'xbcf', 'cf', or 'poisson'"))
  }

  ite_method_inf <- tolower(ite_method_inf)
  if (!(ite_method_inf %in% c("ipw", "sipw", "aipw", "or", "bart", "xbart",
                              "bcf", "xbcf", "cf", "poisson"))) {
    stop(paste("Invalid ITE method for Inference Subsample. Please choose ",
               "from the following: 'ipw', 'sipw', 'aipw', 'or', 'bart', ",
               "'xbart', 'bcf', 'xbcf', 'cf', or 'poisson'"))
  }

  # Check for correct propensity score estimation inputs -----------------------

  include_ps_dis <- toupper(include_ps_dis)
  if (ite_method_dis %in% c("bart", "xbart", "cf")) {
    if (!(include_ps_dis %in% c(TRUE, FALSE))) {
      stop("Please specify 'TRUE' or 'FALSE' for the include_ps_dis argument.")
    }
  } else {
    include_ps_dis <- NA
  }

  include_ps_inf <- toupper(include_ps_inf)
  if (ite_method_inf %in% c("bart", "xbart", "cf")) {
    if (!(include_ps_inf %in% c(TRUE, FALSE))) {
      stop("Please specify 'TRUE' or 'FALSE' for the include_ps_inf argument.")
    }
  } else {
    include_ps_inf <- NA
  }

  if (!(ite_method_dis %in% c("or", "poisson"))) {
    if (!(class(ps_method_dis) %in% c("character", "list"))) {
      stop("Please specify a string or list of strings for the ps_method_dis argument.")
    }
  } else {
    ps_method_dis <- NA
  }

  if (!(ite_method_inf %in% c("or", "poisson"))) {
    if (!(class(ps_method_inf) %in% c("character", "list"))) {
      stop("Please specify a string or list of strings for the ps_method_inf argument.")
    }
  } else {
    ps_method_inf <- NA
  }

  # Check for outcome regression score estimation inputs -----------------------

  if (ite_method_dis %in% c("aipw")) {
    if (!(class(or_method_dis) %in% c("character", "list"))) {
      stop("Please specify a string or list of strings for the or_method_dis argument.")
    }
  } else {
    or_method_dis <- NA
  }

  if (ite_method_inf %in% c("aipw")) {
    if (!(class(or_method_inf) %in% c("character", "list"))) {
      stop("Please specify a string or list of strings for the or_method_inf argument.")
    }
  } else {
    or_method_inf <- NA
  }

  # Determine outcome type
  binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)
  if (binary) {
    if (ite_method_dis %in% c("bcf", "xbcf", "ipw", "sipw") |
        ite_method_inf %in% c("bcf", "xbcf", "ipw", "sipw")) {
      stop(paste("The 'ipw', 'sipw', 'bcf', and 'xbcf' methods are not ",
                 "applicable to data with binary outcomes.Please select a ",
                 "method from the following: 'or', 'cf', 'bart', or 'xbart'"))
    }
  }

  # Check for correct stability_selection and pfer_val input
  if (!(stability_selection %in% c(TRUE, FALSE))) {
    stop("Please specify 'TRUE' or 'FALSE' for the stability_selection argument.")

  }
  if (stability_selection) {
    if (class(pfer_val) != "numeric"){
      stop("Invalid 'pfer_val' input. Please input a number.")
    }
  } else {
    pfer_val <- NA
  }

  # Check for correct offset input
  if ((ite_method_dis == "poisson") | (ite_method_inf == "poisson") | (cate_method == "poisson")) {
    if (!(include_offset %in% c(TRUE, FALSE))) {
      stop("Please specify 'TRUE' or 'FALSE' for the include_offset argument.")
    }
    if (include_offset == TRUE) {
      if (is.na(offset_name)) {
        stop(paste("Invalid offset_name input. Please specify an offset_name ",
                   "if you wish to include an offset."))
      }
    } else {
      offset_name <- NA
    }
  } else {
    include_offset <- FALSE
    offset_name <- NA
  }

  # Check for correct CATE estimation inputs -----------------------

  if (!(cate_method %in% c("poisson", "DRLearner", "bart-baggr", "cf-means",
                           "linreg"))) {
    stop(paste("Invalid CATE method for Inference Subsample. Please choose from ",
               "the following: 'poisson', 'DRLearner', 'bart-baggr', ",
               "'cf-means', or 'linreg'"))
  }

  if (cate_method == "DRLearner") {
    if (!(class(cate_SL_library) %in% c("character", "list"))) {
      stop("Please specify a string or list for the cate_SL_library argument.")
    }
  } else {
    cate_SL_library <- NA
  }

  if (cate_method == "bart-baggr") {
    if (!(ite_method_inf %in% c("bart", "xbart"))) {
      stop(paste("Please choose 'bart' or 'xbart' for ite_method_inf ",
           "if you wish to use 'bart-baggr' as the cate_method"))
    }
  }

  if (cate_method == "cf-means") {
    if (!(ite_method_inf %in% c("cf", "bcf", "xbcf"))) {
      stop(paste("Please choose 'cf', 'bcf', or 'xbcf' for ite_method_inf ",
                 "if you wish to use 'cf-means' as the cate_method"))
    }
  }

  if (!(filter_cate %in% c(TRUE, FALSE))) {
    stop("Invalid 'filter_cate' input. Please specify TRUE or FALSE.")
  }

  # Split data
  logger::log_info("Working on splitting data ... ")
  X_names <- names(as.data.frame(X))
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  subgroups <- split_data(y, z, X, ratio_dis)
  discovery <- subgroups[[1]]
  inference <- subgroups[[2]]

  # Generate y, z, and X for discovery and inference data
  y_dis <- discovery[,1]
  z_dis <- discovery[,2]
  X_dis <- discovery[,3:ncol(discovery)]

  y_inf <- inference[,1]
  z_inf <- inference[,2]
  X_inf <- inference[,3:ncol(inference)]

  # Discovery ------------------------------------------------------------------

  logger::log_info("Conducting Discovery Subsample Analysis ... ")

  # Estimate ITE -----------------------
  logger::log_info("Estimating ITE ... ")
  st_ite_t <- proc.time()
  ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis, ite_method_dis,
                               include_ps_dis, ps_method_dis, or_method_dis,
                               binary, X_names, include_offset, offset_name)
  en_ite_t <- proc.time()
  logger::log_debug("Finished Estimating ITE. ",
                    " Wall clock time: {(en_ite_t - st_ite_t)[[3]]} seconds.")

  ite_dis <- ite_list_dis[["ite"]]
  ite_std_dis <- ite_list_dis[["ite_std"]]

  # Generate rules list ----------------
  logger::log_info("Generating Initial Causal Rules ... ")
  initial_rules_dis <- generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm,
                                      min_nodes, max_nodes)

  # Generate rules matrix --------------
  logger::log_info("Generating Causal Rules Matrix ...")
  rules_all_dis <- generate_rules_matrix(X_dis, initial_rules_dis, t)
  rules_matrix_dis <- rules_all_dis[["rules_matrix"]]
  rules_matrix_std_dis <- rules_all_dis[["rules_matrix_std"]]
  rules_list_dis <- rules_all_dis[["rules_list"]]

  # Select important rules -------------
  logger::log_info("Selecting Important Causal Rules ...")
  select_rules_dis <- as.character(select_causal_rules(rules_matrix_std_dis,
                                                       rules_list_dis,
                                                       ite_std_dis, q,
                                                       stability_selection,
                                                       pfer_val))

  select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in%
                                                       select_rules_dis)]
  select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in%
                                                             select_rules_dis)]

  # Inference ------------------------------------------------------------------

  logger::log_info("Conducting Inference Subsample Analysis ...")
  message("Conducting Inference Subsample Analysis")
  ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf,
                               include_ps_inf, ps_method_inf, or_method_inf,
                               binary, X_names, include_offset, offset_name)
  ite_inf <- ite_list_inf[["ite"]]
  ite_std_inf <- ite_list_inf[["ite_std"]]
  sd_ite_inf <- ite_list_inf[["sd_ite"]]

  if (length(select_rules_dis) == 0){
    # Generate final S3 object
    cate_S3 <- list()
    cate_S3[["ATE_dis"]] <- mean(ite_dis)
    cate_S3[["ATE_inf"]] <- mean(ite_inf)
    cate_S3[["ite_list_dis"]] <- ite_list_dis
    cate_S3[["ite_list_inf"]] <- ite_list_inf
    cate_S3[["outcome_vector_dis"]] <- y_dis
    cate_S3[["treatment_vector_dis"]] <- z_dis
    cate_S3[["covariates_matrix_dis"]] <- X_dis
    cate_S3[["outcome_vector_inf"]] <- y_inf
    cate_S3[["treatment_vector_inf"]] <- z_inf
    cate_S3[["covariates_matrix_inf"]] <- X_inf
    attr(cate_S3, "class") <- "cre"

    # Return Results
    logger::log_info("No significant rules were discovered (iteration 1).")
    return(cate_S3)
  }

  # Estimate CATE ----------------------
  logger::log_info("Estimating CATE ...")
  rules_matrix_inf <- matrix(0, nrow = dim(X_inf)[1],
                             ncol = length(select_rules_dis))
  for (i in 1:length(select_rules_dis)) {
    rules_matrix_inf[eval(parse(text = select_rules_dis[i]),
                          list(X = X_inf)), i] <- 1
  }
  select_rules_interpretable <- interpret_select_rules(select_rules_dis,
                                                       X_names)
  cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names,
                            include_offset, offset_name,
                            rules_matrix_inf, select_rules_interpretable,
                            cate_method, ite_inf, sd_ite_inf,
                            cate_SL_library, filter_cate)

  # Generate final S3 object
  cate_S3 <- list()
  cate_S3[["CATE_results"]] <- cate_inf
  cate_S3[["CATE_method"]] <- cate_method
  cate_S3[["select_rules"]] <- select_rules_interpretable
  cate_S3[["rules_matrix_inf"]] <- rules_matrix_inf
  cate_S3[["outcome_vector_inf"]] <- y_inf
  cate_S3[["treatment_vector_inf"]] <- z_inf
  cate_S3[["covariates_matrix_inf"]] <- X_inf
  cate_S3[["ite_list_inf"]] <- ite_list_inf
  attr(cate_S3, "class") <- "cre"

  # Return Results
  logger::log_info("CRE method complete. Returning results.")
  return(cate_S3)
}
