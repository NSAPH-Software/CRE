# CRE <a href="https://nsaph-software.github.io/CRE/"><img src="man/figures/png/CRE_logo.png" align="right" width="115" /></a>

<!-- badges: start -->
[![](http://www.r-pkg.org/badges/version-last-release/CRE)](https://CRAN.R-project.org/package=CRE)
[![status](https://joss.theoj.org/papers/86a406543801a395248821c08c7ec03d/status.svg)](https://joss.theoj.org/papers/86a406543801a395248821c08c7ec03d)
[![R-CMD-check](https://github.com/fasrc/CRE/workflows/R-CMD-check/badge.svg)](https://github.com/fasrc/CRE/actions)
[![codecov](https://codecov.io/gh/NSAPH-Software/CRE/branch/develop/graph/badge.svg?token=UMSVOYRKGA)](https://codecov.io/gh/NSAPH-Software/CRE)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/CRE)](http://www.r-pkg.org/pkg/cre)
<!-- badges: end -->

# Overview

Identifying subgroups of a study population where a treatment or exposure has a notably larger or smaller effect on an outcome compared to the population average is crucial in social and health sciences. While estimating the conditional average treatment effect (CATE) given a pre-specified set of covariates is a common approach, it only allows for estimating causal effects on subgroups that have been specified a priori by the researchers. This package implements the recently developed Causal Rule Ensemble (CRE) algorithm, a flexible and stable method for interpretable discovery and estimation of heterogeneous treatment effects in terms of decision rules.


# Package Details

The cre() function executes the entire CRE algorithm by utilizing a data set and input parameters the user provides. It divides the study data into discovery and inference subsamples, generating and filtering decision rules on the discovery subsample to explain the heterogeneity in treatment effects. cre() uses the inference subsample to estimate a linear decomposition of the Conditional Average Treatment Effect (CATE) in terms of the just discovered decision rules, by normal equations to model some ITE estimates. In both steps, cre() is agnostic concerning the method used for ITE estimation. To estimate Individual Treatment Effects (ITE), users can choose from various methodologies that vary in terms of speed and accuracy. Several ITE estimators are internally implemented (i.e. AIPW, Causal Forest, Bayesian Causal Forest, Causal BART, S-Learner, T-Learner, X-Learner), but a customized ITE estimation can also be provided in input.
The results are collected in an S3 object containing: the number of decision rules extracted at each step, the CATE decomposition estimates with corresponding uncertainty quantification, the list of selected parameters, and the predicted ITEs. 


# Applications

The CRE package can be used in diverse theoretical and practical applications as long as working with binary exposure or treatment level. For example, it can be used in education to determine which teaching methods are most effective for specific student subgroups. In marketing, it can identify the most effective strategies for different consumer subgroups. In healthcare, it can be used to identify patient subgroups that benefit the most from a particular treatment or intervention. It can also be used in studies for personalized medicine where each subgroup of patients responds differently to the provided medicine. In public health, the CRE method was applied to discover subgroups that are more vulnerable (or resilient) to the causal effects of long-term exposure to air pollution on mortality.
