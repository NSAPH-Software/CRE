#' @title
#' Estimate the Conditional Average Treatment Effect
#'
#' @description
#' Estimates the Conditional Average Treatment Effect given a standardized
#' vector of Individual Treatment Effects, a standardized matrix of causal rules,
#' a list of causal rules.
#'
#' @param y_inf the outcome vector for the inference subsample
#' @param z_inf the treatment vector for the inference subsample
#' @param X_inf the covariate vector for the inference subsample
#' @param X_names the names of the covariates
#' @param include_offset whether or not to include an offset when estimating the
#'  ITE, for poisson only
#' @param offset_name the name of the offset, if it is to be included
#' @param rules_matrix_inf the standardized causal rules matrix for the
#' inference subsample
#' @param select_rules_interpretable the list of select causal rules in terms of
#' coviariate names
#' @param cate_method the method to estimate the CATE values
#' @param ite_inf the estimated ITEs for the inference subsample
#' @param sd_ite_inf the standard deviations for the estimated ITEs for the
#'   inference subsample
#' @param cate_SL_library the library used if cate_method = DRLearner
#' @param filter_cate whether or not to filter rules with p-value <= 0.05
#'
#' @return
#' a matrix of CATE estimates
#'
#' @import stats
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
#' ite_method_inf <- "bart"
#' include_ps_inf <- TRUE
#' ps_method_inf <- "SL.xgboost"
#' or_method_inf <- NA
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
#' cate_method <- "DRLearner"
#' cate_SL_library <- "SL.xgboost"
#' filter_cate <- FALSE
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
#' y_inf <- inference[,1]
#' z_inf <- inference[,2]
#' X_inf <- inference[,3:ncol(inference)]
#'
#' # Estimate ITE on Discovery Subsample
#' ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis, ite_method_dis,
#'                              include_ps_dis, ps_method_dis, or_method_dis,
#'                              binary, X_names, include_offset, offset_name)
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
#'                                                      ite_std_dis, binary, q, rules_method))
#' select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in% select_rules_dis)]
#' select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in% select_rules_dis)]
#' if (length(select_rules_dis) == 0) stop("No significant rules were discovered. Ending Analysis.")
#'
#' # Estimate Inference ITE and CATE
#' rules_matrix_inf <- matrix(0, nrow = dim(X_inf)[1], ncol = length(select_rules_dis))
#' for (i in 1:length(select_rules_dis)) {
#'   rules_matrix_inf[eval(parse(text = select_rules_dis[i]), list(X = X_inf)), i] <- 1
#' }
#' select_rules_interpretable <- interpret_select_rules(select_rules_dis, X_names)
#'
#' ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf,
#'                              include_ps_inf, ps_method_inf, or_method_inf,
#'                              binary, X_names, include_offset, offset_name)
#' ite_inf <- ite_list_inf[["ite"]]
#' ite_std_inf <- ite_list_inf[["ite_std"]]
#'
#' cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names, include_offset, offset_name,
#'                          rules_matrix_inf, select_rules_interpretable,
#'                          cate_method, ite_inf, sd_ite_inf,
#'                          cate_SL_library, filter_cate)
#'
estimate_cate <- function(y_inf, z_inf, X_inf, X_names, include_offset,
                          offset_name, rules_matrix_inf,
                          select_rules_interpretable,
                          cate_method, ite_inf, sd_ite_inf,
                          cate_SL_library, filter_cate) {

  # Handling global variable error.
  `%>%` <- magrittr::`%>%`
  Rule <- rule <- tau <- se <- . <- Estimate <- NULL

  if (cate_method %in% c("poisson")) {
    colnames(rules_matrix_inf) <- select_rules_interpretable
    colnames(X_inf) <- X_names
    if (include_offset) {
      X_offset <- X_inf[,which(X_names == offset_name)]
      X_inf <- X_inf[,-which(X_names == offset_name)]

      # Fit gnm model
      conditional_gnm <- gnm::gnm(y_inf ~ offset(log(X_offset)) + z_inf +
                                    z_inf:rules_matrix_inf + X_inf,
                                  family = stats::poisson(link = "log"))
    } else {
      # Fit gnm model
      conditional_gnm <- gnm::gnm(y_inf ~ z_inf + z_inf:rules_matrix_inf + X_inf,
                                  family = stats::poisson(link = "log"))
    }
    cate_model <- summary(conditional_gnm)$coefficients
    cate_names <- rownames(cate_model) %>%
      stringr::str_remove_all("X_inf") %>%
      stringr::str_remove_all("rules_matrix_inf") %>%
      stringr::str_replace_all("z_inf", "treatment")

    cate_temp <- data.frame(Predictor = cate_names) %>%
      cbind(cate_model)
    colnames(cate_temp) <- c("Predictor", "Estimate", "Std_Error",
                             "Z_Value", "P_Value")
    if (filter_cate) {
      cate_final <- subset(cate_temp, cate_temp$P_Value <= 0.05)
    } else {
      cate_final <- cate_temp
    }
    rownames(cate_final) <- 1:nrow(cate_final)
  } else if (cate_method %in% c("DRLearner")) {
    # split the data evenly
    split <- sample(nrow(X_inf), nrow(X_inf) * 0.5, replace = FALSE)

    # assign names to rules matrix and X matrix
    colnames(rules_matrix_inf) <- select_rules_interpretable
    colnames(X_inf) <- X_names

    # generate new data frames
    y_inf_a <- y_inf[split]
    y_inf_b <- y_inf[-split]
    X_inf_a <- as.data.frame(X_inf[split,])
    X_inf_b <- as.data.frame(X_inf[-split,])
    z_inf_a <- z_inf[split]
    z_inf_b <- z_inf[-split]
    rules_matrix_inf_a <- rules_matrix_inf[split,]
    rules_matrix_inf_b <- rules_matrix_inf[-split,]

    # on set A, train a model to predict Z using X, then make predictions on set B
    sl_z <- SuperLearner::SuperLearner(Y = z_inf_a, X = X_inf_a, newX = X_inf_b,
                                       family = binomial(),
                                       SL.library = cate_SL_library,
                                       cvControl = list(V=0))
    phat <- sl_z$SL.predict

    # generate CATE estimates for set A, predict set B
    sl_y <- SuperLearner::SuperLearner(Y = y_inf_a,
                                       X = data.frame(X = X_inf_a, Z = z_inf_a),
                                       family = gaussian(),
                                       SL.library = cate_SL_library,
                                       cvControl = list(V=0))
    pred_0s <- stats::predict(sl_y,
                              data.frame(X = X_inf_b, Z = rep(0, nrow(X_inf_b))),
                              onlySL = TRUE)
    pred_1s <- stats::predict(sl_y,
                              data.frame(X = X_inf_b, Z = rep(1, nrow(X_inf_b))),
                              onlySL = TRUE)

    cate <- pred_1s$pred - pred_0s$pred

    # generate AIPW estimate
    apo_1 <- pred_1s$pred + (z_inf_b*(y_inf_b - pred_1s$pred)/(phat))
    apo_0 <- pred_0s$pred + ((1 - z_inf_b)*(y_inf_b - pred_0s$pred)/(1-phat))

    delta <- apo_1 - apo_0

    # regress AIPW onto the rules
    DRLearner <- stats::lm(delta ~ rules_matrix_inf_b)
    cate_model <- summary(DRLearner)$coefficients
    colnames(cate_model) <- c("Estimate", "Std_Error", "Z_Value", "P_Value")
    cate_names <- rownames(cate_model) %>%
      stringr::str_remove_all("rules_matrix_inf_b") %>%
      stringr::str_replace_all("(Intercept)", "Treatment")

    cate_temp <- data.frame(Predictor = cate_names) %>%
      cbind(cate_model)
    if (filter_cate) {
      cate_final <- subset(cate_temp, cate_temp$P_Value <= 0.05)
    } else {
      cate_final <- cate_temp
    }
    rownames(cate_final) <- 1:nrow(cate_final)
  } else if (cate_method == "bart-baggr") {
    stopifnot(ncol(rules_matrix_inf) == length(select_rules_interpretable))
    df_rules_factor <- as.data.frame(rules_matrix_inf) %>%
      dplyr::transmute_all(as.factor)
    names(df_rules_factor) <- select_rules_interpretable
    joined_ite <- data.frame(group = 1:length(ite_inf),
                             tau = ite_inf, se = sd_ite_inf)

    # Generate CATE data frame with ATE
    options(mc.cores = parallel::detectCores())
    baggr_ite <- suppressWarnings(
      baggr::baggr(joined_ite, cores = getOption("mc.cores",
                                                 parallel::detectCores())))
    sum_ate <- 0
    sum_sd_ate <- 0
    n_samples <- length(baggr_ite$fit@sim$samples)
    for (i in 1:n_samples) {
      sum_ate <- sum_ate + mean(baggr_ite$fit@sim$samples[[i]]$`mu[1]`)
      sum_sd_ate <- sum_sd_ate + mean(baggr_ite$fit@sim$samples[[i]]$`tau[1]`)
    }

    ate <- sum_ate / n_samples
    sd_ate <- sum_sd_ate / n_samples
    cate_means <- data.frame(Rule = "Average Treatment Effect",
                             CATE = ate,
                             CI_lower = ate - (1.96 * sd_ate),
                             CI_upper = ate + (1.96 * sd_ate))

    # Determine CATE manually for each rule
    for (i in 1:length(select_rules_interpretable)) {
      df_temp <- data.frame(tau = ite_inf, se = sd_ite_inf,
                            rule = df_rules_factor[,i]) %>%
        dplyr::filter(rule == 1) %>% dplyr::select(-rule)
      df_temp <- df_temp %>% dplyr::summarize(group = 1:nrow(df_temp), tau, se)
      options(mc.cores = parallel::detectCores())
      baggr_ite_temp <- suppressWarnings(
        baggr::baggr(df_temp, cores = getOption("mc.cores",
                                                parallel::detectCores())))

      sum_cate_temp <- 0
      sum_sd_cate_temp <- 0
      n_samples_temp <- length(baggr_ite_temp$fit@sim$samples)
      for (j in 1:n_samples_temp) {
        sum_cate_temp <- sum_cate_temp +
          mean(baggr_ite_temp$fit@sim$samples[[j]]$`mu[1]`)
        sum_sd_cate_temp <- sum_sd_cate_temp +
          mean(baggr_ite_temp$fit@sim$samples[[j]]$`tau[1]`)
      }
      cate_temp <- sum_cate_temp / n_samples_temp
      sd_cate_temp <- sum_sd_cate_temp / n_samples_temp

      cate_temp <- data.frame(Rule = select_rules_interpretable[i],
                              CATE = cate_temp,
                              CI_lower = cate_temp - (1.96 * sd_cate_temp),
                              CI_upper = cate_temp + (1.96 * sd_cate_temp))
      cate_means <- rbind(cate_means, cate_temp)
    }
    cate_final <- cate_means
  } else if (cate_method == "cf-means") {
    stopifnot(ncol(rules_matrix_inf) == length(select_rules_interpretable))
    df_rules_factor <- as.data.frame(rules_matrix_inf) %>%
      dplyr::transmute_all(as.factor)
    names(df_rules_factor) <- select_rules_interpretable
    joined_ite_rules <- cbind(ite_inf, sd_ite_inf, df_rules_factor)

    # Generate CATE data frame with ATE
    cate_means <- data.frame(Rule = "Average Treatment Effect",
                             CATE = mean(ite_inf),
                             CI_lower = mean(ite_inf) -
                               (1.96 * mean(sd_ite_inf)),
                             CI_upper = mean(ite_inf) +
                               (1.96 * mean(sd_ite_inf)))

    # Determine CATE manually for each rule
    for (i in 1:length(select_rules_interpretable)) {
      df_temp <- data.frame(ite_inf = joined_ite_rules[,1],
                            sd_ite_inf = joined_ite_rules[,2],
                            rule = joined_ite_rules[,i + 2]) %>%
        dplyr::filter(rule == 1)
      cate_temp <- data.frame(Rule = select_rules_interpretable[i],
                              CATE = mean(df_temp$ite),
                              CI_lower = mean(df_temp$ite_inf) -
                                (1.96 * mean(df_temp$sd_ite_inf)),
                              CI_upper = mean(df_temp$ite_inf) +
                                (1.96 * mean(df_temp$sd_ite_inf)))
      cate_means <- rbind(cate_means, cate_temp)
    }
    cate_final <- cate_means
  } else if (cate_method == "linreg") {
    stopifnot(ncol(rules_matrix_inf) == length(select_rules_interpretable))
    df_rules_factor <- as.data.frame(rules_matrix_inf) %>%
      dplyr::transmute_all(as.factor)
    names(df_rules_factor) <- select_rules_interpretable
    joined_ite_rules <- cbind(ite_inf, df_rules_factor)

    # Fit linear regression model with contr.treatment
    # then extract coefficients and confidence intervals
    options(contrasts = rep("contr.treatment", 2))
    model1_cate <- stats::lm(ite_inf ~ ., data = joined_ite_rules)
    model1_coef <- summary(model1_cate)$coef[,c(1,4)] %>% as.data.frame
    model1_ci <- stats::confint(model1_cate) %>% as.data.frame() %>%
      dplyr::filter(!is.na(.))

    # Generate model 1 data frame
    cate_reg_orig <- model1_coef %>% cbind(model1_ci)
    cate_reg_orig_names <- stringr::str_extract(row.names(cate_reg_orig), "`.*`") %>%
      stringr::str_remove_all("`")
    cate_reg_orig_names[1] <- "(Intercept)"
    cate_reg_orig$Rule <- cate_reg_orig_names
    row.names(cate_reg_orig) <- 1:nrow(cate_reg_orig)
    cate_reg_orig <- cate_reg_orig %>%
      dplyr::summarize(Rule, Estimate, P_Value = "Pr(>|t|)",
                       CI_lower = "2.5 %", CI_upper = "97.5 %")
    if (filter_cate) {
      cate_final <- subset(cate_reg_orig, cate_reg_orig$P_Value <= 0.05)
    } else {
      cate_final <- cate_reg_orig
    }
  } else {
    stop("Error: No CATE Estimation method specified.")
  }

  # Return final results
  return(cate_final)
}
