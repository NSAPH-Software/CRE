#' @title
#' Print CRE Results
#'
#' @description
#' Prints a brief description of the CRE object
#'
#' @param cre_object the output cre object from running the CRE function
#'
#' @return
#' A print statement of the CRE object
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
#' print_cre(cre_results)
#'
print_cre <- function(cre_object) {
  stopifnot(class(cre_object) == "cre")
  if ("CATE_results" %in% names(cre_object)) {
    print(paste("CRE results using CATE method: ", cre_object[["CATE_method"]]))
    print(cre_object[["CATE_results"]])
  } else {
    print("No significant rules were discovered.")
  }
}
