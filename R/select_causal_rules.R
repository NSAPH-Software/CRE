#' @title
#' Select Causal Rules
#'
#' @description
#' Selects the causal rules that are most important.
#'
#' @param rules_matrix_std the standardized causal rules matrix
#' @param rules_list a vector of causal rules
#' @param ite_std the standardized ITE
#' @param stability_selection whether or not using stability selection to select the causal rules
#' @param q the selection threshold used in selecting the causal rules
#' @param rules_method the method for selecting causal rules with binary
#'  outcomes, either "conservative", "anticonservative", or NA
#'
#' @return
#' a vector of causal rules
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
#' X_names <- names(as.data.frame(X))
#' ratio_dis <- 0.25
#' ite_method_dis <- "bart"
#' include_ps_dis <- TRUE
#' ps_method_dis <- "SL.xgboost"
#' or_method_dis <- NA
#' ntrees_rf <- 100
#' ntrees_gbm <- 50
#' min_nodes <- 20
#' max_nodes <- 5
#' t <- 0.025
#' q <- 0.8
#' rules_method <- NA
#' include_offset <- FALSE
#' offset_name <- NA
#' binary <- FALSE
#'
#' # Split data
#' X <- as.matrix(X)
#' y <- as.matrix(y)
#' z <- as.matrix(z)
#' subgroups <- split_data(y, z, X, ratio_dis)
#' discovery <- subgroups[[1]]
#' inference <- subgroups[[2]]
#'
#' # Generate y, z, and X for discovery and inference data
#' y_dis <- discovery[,1]
#' z_dis <- discovery[,2]
#' X_dis <- discovery[,3:ncol(discovery)]
#'
#' # Estimate ITE on Discovery Subsample
#' ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis, ite_method_dis, include_ps_dis,
#'                              ps_method_dis, or_method_dis, binary, X_names,
#'                              include_offset, offset_name)
#' ite_dis <- ite_list_dis[["ite"]]
#' ite_std_dis <- ite_list_dis[["ite_std"]]
#'
#' # Generate rules list
#' initial_rules_dis <- generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm,
#'                                     min_nodes, max_nodes)
#'
#' # Generate rules matrix
#' rules_all_dis <- generate_rules_matrix(X_dis, initial_rules_dis, t)
#' rules_matrix_dis <- rules_all_dis[["rules_matrix"]]
#' rules_matrix_std_dis <- rules_all_dis[["rules_matrix_std"]]
#' rules_list_dis <- rules_all_dis[["rules_list"]]
#'
#' # Select important rules
#' select_rules_dis <- as.character(select_causal_rules(rules_matrix_std_dis, rules_list_dis,
#'                                                      ite_std_dis, stability_selection, q, rules_method))
#'
select_causal_rules <- function(rules_matrix_std, rules_list, ite_std, stability_selection, q, rules_method) {

  `%>%` <- magrittr::`%>%`
  rules <- NULL

  if (stability_selection) {
    # Stability selection
    stab_mod <- stabs::stabsel(rules_matrix_std, ite_std, fitfun = "glmnet.lasso", cutoff = q,
                               PFER = 0.1, args.fitfun = "conservative")
    rule_stab <- rules_list[stab_mod$selected]
    select_rules <- rule_stab
  } else {
    # LASSO
    cv_lasso <- glmnet::cv.glmnet(rules_matrix_std, ite_std, alpha = 1, intercept = FALSE)
    aa <- stats::coef(cv_lasso, s = cv_lasso$lambda.1se)
    index_aa <- which(aa[-1,1] != 0)
    rule_LASSO <- data.frame(rules = rules_list[index_aa], val = aa[index_aa + 1, 1])
    rule_LASSO <- rule_LASSO[order(-rule_LASSO[,2]), ] %>% dplyr::filter(!is.na(rules))
    select_rules <- rule_LASSO$rules
  }
  return(select_rules)
}
