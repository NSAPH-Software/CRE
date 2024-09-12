# CRE

<p align="center">
  <img src="man/figures/png/CRE_logo.png" height="100" alt="Cover Image"/>
</p>


<div align="center">
    <a href="https://CRAN.R-project.org/package=CRE">
        <img src="http://www.r-pkg.org/badges/version-last-release/CRE" alt="CRAN Package Version">
    </a>
    <a href="https://joss.theoj.org/papers/86a406543801a395248821c08c7ec03d">
        <img src="https://joss.theoj.org/papers/86a406543801a395248821c08c7ec03d/status.svg" alt="JOSS Status">
    </a>
    <a href="https://github.com/nsaph-software/CRE/actions">
        <img src="https://github.com/nsaph-software/CRE/workflows/R-CMD-check/badge.svg" alt="R-CMD-check Status">
    </a>
    <a href="https://app.codecov.io/gh/NSAPH-Software/CRE">
        <img src="https://codecov.io/gh/NSAPH-Software/CRE/branch/develop/graph/badge.svg?token=UMSVOYRKGA" alt="Codecov">
    </a>
    <a href="http://www.r-pkg.org/pkg/cre">
        <img src="https://cranlogs.r-pkg.org/badges/grand-total/CRE" alt="CRAN RStudio Mirror Downloads">
    </a>
</div>





#### Interpretable Discovery and Inference of Heterogeneous Treatment Effects

In health and social sciences, it is critically important to identify subgroups of the study population where a treatment has notable heterogeneity in the causal effects with respect to the average treatment effect (ATE). The bulk of heterogeneous treatment effect (HTE) literature focuses on two major tasks: (i) estimating HTEs by examining the conditional average treatment effect (CATE); (ii) discovering subgroups of a population characterized by HTE. 

Several methodologies have been proposed for both tasks, but providing interpretability in the results is still an open challenge. [Bargagli-Stoffi et al. (2023)](https://arxiv.org/abs/2009.09036) proposed Causal Rule Ensemble, a new method for HTE characterization in terms of decision rules, via an extensive exploration of heterogeneity patterns by an ensemble-of-trees approach, enforcing stability in the discovery. CRE is an R Package providing a flexible implementation of the Causal Rule Ensemble algorithm.


## Installation

Installing from CRAN.

```r
install.packages("CRE")
```

Installing the latest developing version. 

```r
library(devtools)
install_github("NSAPH-Software/CRE", ref="develop")
```

Import.

```r
library("CRE")
```
The full list of required dependencies can be found in project in the DESCRIPTION file.

## Arguments

__Data (required)__   
**`y`** The observed response/outcome vector (binary or continuous).

**`z`** The treatment/exposure/policy vector (binary).  

**`X`** The covariate matrix (binary or continuous).    

__Parameters (not required)__    
**`method_parameters`** The list of parameters to define the models used, including:
- **`ratio_dis`** The ratio of data delegated to the discovery sub-sample (default: 0.5). 
- **`ite_method`** The method to estimate the individual treatment effect (ITE) pseudo-outcome estimation (default: "aipw") [1].        
- **`learner_ps`** The SuperLearner model for the propensity score estimation (default: "SL.xgboost", used only for "aipw","bart","cf" ITE estimators).
- **`learner_y`** The SuperLearner model for the outcome estimation (default: "SL.xgboost", used only for "aipw","slearner","tlearner" and "xlearner" ITE estimators).   

**`hyper_params`** The list of hyper parameters to finetune the method, including:
- **`intervention_vars`** Array with intervention-able covariates names used for Rules Generation. Empty or null array means that all the covariates are considered as intervention-able (default: `NULL`).  
- **`ntrees`** The number of decision trees for random forest (default: 20).   
- **`node_size`** Minimum size of the trees' terminal nodes (default: 20).
- **`max_rules`** Maximum number of generated candidates rules (default: 50).
- **`max_depth`** Maximum rules length (default: 3).  
- **`t_decay`** The decay threshold for rules pruning (default: 0.025).          
- **`t_ext`** The threshold to define too generic or too specific (extreme) rules (default: 0.01).     
- **`t_corr`** The threshold to define correlated rules (default: 1). 
- **`stability_selection`** Method for stability selection for selecting the rules. "vanilla" for stability selection, "error_control" for stability selection with error control and "no" for no stability selection (default: "vanilla").
- **`B`** Number of bootstrap samples for stability selection in rules selection and uncertainty quantification in estimation (default: 20).
- **`subsample`** Bootstrap ratio subsample and stability selection in rules selection, and uncertainty quantification in estimation (default: 0.5).
- **`offset`** Name of the covariate to use as offset (i.e. "x1") for T-Poisson ITE Estimation. `NULL` if not used (default: `NULL`).   
- **`cutoff`** Threshold defining the minimum cutoff value for the stability scores in Stability Selection (default: 0.9).    
- **`pfer`** Upper bound for the per-family error rate (tolerated amount of falsely selected rules) in Error Control Stability Selection (default: 1).

__Additional Estimates (not required)__    
**`ite`** The estimated ITE vector. If given, both the ITE estimation steps in Discovery and Inference are skipped (default: `NULL`).


## Notes

**[1]** Options for the ITE estimation are as follows: 
- S-Learner (`slearner`)
- T-Learner (`tlearner`)
- T-Poisson (`tpoisson`)
- X-Learner (`xlearner`)
- Augmented Inverse Probability Weighting (`aipw`)
- Causal Forests (`cf`)
- Causal Bayesian Additive Regression Trees (`bart`)

If other estimates of the ITE are provided in `ite` additional argument, both the ITE estimations in discovery and inference are skipped and those values estimates are used instead.  The ITE estimator requires also an outcome learner and/or a propensity score learner from the SuperLearner package (i.e., "SL.lm", "SL.svm"). Both these models are simple classifiers/regressors. By default XGBoost algorithm is used for both these steps.


## Examples

**Example 1** (*default parameters*)
```R
set.seed(2023)
dataset <- generate_cre_dataset(n = 2000, 
                                rho = 0, 
                                n_rules = 2, 
                                p = 10,
                                effect_size = 5, 
                                binary_covariates = TRUE,
                                binary_outcome = FALSE,
                                confounding = "no")
y <- dataset[["y"]]
z <- dataset[["z"]]
X <- dataset[["X"]]

cre_results <- cre(y, z, X)
summary(cre_results)
plot(cre_results)
ite_pred <- predict(cre_results, X) 
```

**Example 2** (*personalized ite estimation*)
```R
set.seed(2023)
dataset <- generate_cre_dataset(n = 2000, 
                                rho = 0, 
                                n_rules = 2, 
                                p = 10,
                                effect_size = 5, 
                                binary_covariates = TRUE,
                                binary_outcome = FALSE,
                                confounding = "no")
  y <- dataset[["y"]]
  z <- dataset[["z"]]
  X <- dataset[["X"]]

# personalized ITE estimation (S-Learner with Linear Regression)
model <- lm(y ~., data = data.frame(y = y, X = X, z = z))
ite_pred <- predict(model, newdata = data.frame(X = X, z = z))

cre_results <- cre(y, z, X, ite = ite_pred)
summary(cre_results)
plot(cre_results)
ite_pred <- predict(cre_results, X)
```

**Example 3** (*setting parameters*)
```R
  set.seed(2023)
  dataset <- generate_cre_dataset(n = 2000, 
                                  rho = 0, 
                                  n_rules = 2, 
                                  p = 10,
                                  effect_size = 2, 
                                  binary_covariates = TRUE,
                                  binary_outcome = FALSE,
                                  confounding = "no")
  y <- dataset[["y"]]
  z <- dataset[["z"]]
  X <- dataset[["X"]]

  method_params = list(ratio_dis = 0.5,
                       ite_method ="aipw",
                       learner_ps = "SL.xgboost",
                       learner_y = "SL.xgboost")

 hyper_params = list(intervention_vars = c("x1","x2","x3","x4","x5","x6"),
                     offset = NULL,
                     ntrees = 20,
                     node_size = 20,
                     max_rules = 50,
                     max_depth = 2,
                     t_decay = 0.025,
                     t_ext = 0.025,
                     t_corr = 1,
                     stability_selection = "vanilla",
                     cutoff = 0.8,
                     pfer = 0.1,
                     B = 50,
                     subsample = 0.1)

cre_results <- cre(y, z, X, method_params, hyper_params)
summary(cre_results)
plot(cre_results)
ite_pred <- predict(cre_results, X)
```

More synthetic data sets can be generated using `generate_cre_dataset()`.


## Simulations
Reproduce simulation experiments in Section 4 in @bargagli2023causal, evaluating Causal Rule Ensemble Discovery and Estimation performances, comparing with different benchmarks. 

**Discovery**: Evaluate performance of Causal Rule Ensemble algorithm (varying the pseudo-outcome estimator) in rules and effect modifier discovery.

```r
CRE/functional_tests/experiments/discovery.R
```

**Estimation**: Evaluate performance of Causal Rule Ensemble algorithm (varying the pseudo-outcome estimator) in treatment effect estimation and comparing it with the corresponding stand-alone ITE estimators.

```r
CRE/functional_tests/experiments/estimation.R
```

More exhaustive simulation studies and real world experiment of CRE package can be found at [https://github.com/NSAPH-Projects/cre_applications](https://github.com/NSAPH-Projects/cre_applications).


## Code of Conduct

Please note that the CRE project is released with a [Contributor Code of Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct). By contributing to this project, you agree to abide by its terms. More information about the opening issues and contributing (i.e., git branching model) can be found on [CRE website](https://nsaph-software.github.io/CRE/articles/Contribution.html).

