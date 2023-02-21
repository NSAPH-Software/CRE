#' @title
#' Estimate the Conditional Average Treatment Effect
#'
#' @description
#' Estimates the Conditional Average Treatment Effect (CATE) by
#' linearly modeling the Individual Treatment Effect by a set of rules.
#'
#' @param rules_matrix A rules matrix,
#' @param rules_explicit A list of select rules in terms of covariate names.
#' @param ite The estimated ITEs.
#' @param t_pvalue The threshold to define statistically significant rules
#' (filter only rules with p-value <= t_pvalue).
#'
#' @return
#' A list with 2 elements:
#' `summary`: A dataframe summarizing the CATE linear decomposition:
#' - 'Rule': rule name,
#' - 'Estimate': linear contribution to CATE,
#' - 'CI_lower`: lower bound 95% confidence interval on the estimate,
#' - 'CI_upper`: upper bound 95% confidence interval on the estimate,
#' - `P-Value`: p-value (from Z-test).
#' `model`: A linear model for CATE-ATE estimation.
#'
#' @import stats
#' @keywords internal
#'
#'
estimate_cate <- function(rules_matrix, rules_explicit, ite, t_pvalue) {

  "%>%" <- magrittr::"%>%"

  logger::log_debug("Estimating CATE ... ")

  # Estimate ATE (if No Rules Selected)
  ate_model <- stats::lm(ite ~ 1)
  ate_coeff <- summary(ate_model)$coefficients
  ate_ci <- stats::confint(ate_model)
  ate_summary <- data.frame(Rule = "(ATE)",
                            Estimate = ate_coeff[1],
                            CI_lower = ate_ci[1],
                            CI_upper = ate_ci[2],
                            P_Value = ate_coeff[2])
  rownames(ate_summary) <- 1:nrow(ate_summary)
  if (length(rules_explicit) == 0) {
    cate_summary <- ate_summary
    aate_model <- NA
  } else {
    # Estimate AATEs
    rules_df_inf <- as.data.frame(rules_matrix)
    names(rules_df_inf) <- rules_explicit
    aate_model <- stats::lm(ite - mean(ite) ~ . -1, data = rules_df_inf)
    filter_na <- is.na(aate_model$coefficients)
    if (sum(filter_na)) {
      rules_matrix <- rules_matrix[, !filter_na]
      rules_explicit <- rules_explicit[!filter_na]
      return(estimate_cate(rules_matrix, rules_explicit, ite, t_pvalue))
    }
    aate_coeff <- summary(aate_model)$coef[, c(1, 4), drop = FALSE] %>%
                  as.data.frame()
    aate_ci <- stats::confint(aate_model) %>% as.data.frame()
    aate_summary <- data.frame(Rule = rules_explicit,
                               Estimate = aate_coeff[, 1],
                               CI_lower = aate_ci[, 1],
                               CI_upper = aate_ci[, 2],
                               P_Value = aate_coeff[, 2])
    if (t_pvalue < 1) {
      filter_pvalue <- aate_summary$P_Value <= t_pvalue
      if (sum(filter_pvalue) < length(filter_pvalue)) {
        rules_matrix <- rules_matrix[, filter_pvalue, drop = FALSE]
        rules_explicit <- rules_explicit[filter_pvalue]
        return(estimate_cate(rules_matrix, rules_explicit, ite, t_pvalue))
      }
    }
    cate_summary <- rbind(ate_summary, aate_summary)
    rownames(cate_summary) <- seq_along(1:nrow(cate_summary))

    # Filter Not Significant Rules
    # TODO: This section needs refactoring. Calling the function itself, while
    # while the nature of the function is not recursive is not a good practice.
  }
  logger::log_debug("Done with estimating CATE.")

  return(list(summary = cate_summary, model = aate_model))
}
