#' @title
#' Generate rules
#'
#' @description
#' Generates a list of rules characterizing the heterogeneity in the Conditional
#' Average Treatment Effect (CATE) by tree-based metods: random forest (RF) and
#' generalized boosted regression modeling (GBM).
#'
#' @param X A covariate matrix.
#' @param ite A vector of estimated ITE.
#' @param ntrees_rf A number of decision trees for the random forest algorithm.
#' @param ntrees_gbm A number of decision trees for the generalized boosted
#' regression modeling algorithm.
#' @param node_size Minimum size of the trees' terminal nodes.
#' @param max_nodes Maximum number of terminal nodes per tree.
#' @param max_depth Maximum rules length.
#' @param replace A boolean variable for replacement in bootstrapping.
#'
#' @return
#' A list of rules (names).
#'
#' @keywords internal
#'
generate_rules <- function(X, ite, ntrees_rf, ntrees_gbm,
                           node_size, max_nodes, max_depth, replace) {

  logger::log_debug("Generating (candidate) rules...")
  st_time <- proc.time()

  # Random Forest
  # TODO: replace splitting criteria enforcing heterogeneity
  if (ntrees_rf > 0) {
    N <- dim(X)[1]
    sampsize <- min(1, (11 * sqrt(N) + 1) / N) * N
    forest_RF <- randomForest::randomForest(x = X,
                                            y = ite,
                                            sampsize = sampsize,
                                            replace = replace,
                                            ntree = ntrees_rf,
                                            maxnodes = max_nodes,
                                            nodesize = node_size)

    treelist_RF <- inTrees::RF2List(forest_RF)
    rules_RF <- extract_rules(treelist_RF, X, max_depth)
  } else {
    rules_RF <- NULL
  }

  # Gradient Boosting
  if (ntrees_gbm > 0) {
    mn <- max_nodes
    forest_GB <- gbm::gbm(ite ~ .,
                          data = as.data.frame(X),
                          n.trees = ntrees_gbm,
                          interaction.depth = max_depth,
                          n.minobsinnode = node_size,
                          distribution = "gaussian",
                          verbose = FALSE)

    treelist_GB <- inTrees::GBM2List(forest_GB, X)
    rules_GB <- extract_rules(treelist_GB, X, max_depth)
  } else {
    rules_GB <- NULL
  }

  rules <- unique(c(rules_RF, rules_GB))
  en_time <- proc.time()
  logger::log_debug("Done with generating (candidate) rules. ",
                    "(WC: {g_wc_str(st_time, en_time)}", ".)")
  return(rules)
}
