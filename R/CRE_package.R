#' @title
#' The CRE package
#'
#' @description
#' In health and social sciences, it is critically important to
#' identify subgroups of the study population where a treatment
#' has notable heterogeneity in the causal effects with respect
#' to the average treatment effect. Data-driven discovery of
#' heterogeneous treatment effects (HTE) via decision tree methods
#' has been proposed for this task. Despite its high interpretability,
#' the single-tree discovery of HTE tends to be highly unstable and to
#' find an oversimplified representation of treatment heterogeneity.
#' To accommodate these shortcomings, we propose Causal Rule Ensemble
#' (CRE), a new method to discover heterogeneous subgroups through an
#' ensemble-of-trees approach. CRE has the following features:
#' 1) provides an interpretable representation of the HTE; 2) allows
#' extensive exploration of complex heterogeneity patterns; and 3)
#' guarantees high stability in the discovery. The discovered subgroups
#' are defined in terms of interpretable decision rules, and we develop
#' a general two-stage approach for subgroup-specific conditional
#' causal effects estimation, providing theoretical guarantees.
#'
#' @name CRE-package
#' @aliases CRE
#' @author Naeem Khoshnevis
#' @author Daniela Maria Garcia
#' @author Riccardo Cadei
#' @author Kwonsang Lee
#' @author Falco Joannes Bargagli Stoffi
#' @import xtable
#' @import data.table
#' @import SuperLearner
#' @importFrom RRF RRF
#' @importFrom RRF getTree
#' @importFrom gbm pretty.gbm.tree
#' @importFrom xgboost xgb.model.dt.tree
#' @import stats
#' @importFrom methods as
#'
#' @references
#' Bargagli-Stoffi, F. J., Cadei, R., Lee, K. and Dominici, F. (2023).
#' Causal rule ensemble: Interpretable Discovery and Inference of
#' Heterogeneous Treatment Effects,arXiv preprint arXiv:2009.09036
#'
"_PACKAGE"
