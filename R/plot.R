#' @title
#' A helper function for cre object
#'
#' @description
#' A helper function to plot cre object using ggplot2 package.
#'
#' @param object A cre object.
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' Returns a ggplot object.
#'
#' @keywords internal
#' @importFrom ggplot2 autoplot
#'
autoplot.cre <- function(object, ...){

  gg_labs <- gg_title <- NULL

  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  cate_results <- object[["CATE_results"]]
  cate_method <- object[["CATE_method"]]

  # Handling global variable error.
  `%>%` <- magrittr::`%>%`
  Predictor <- Estimate <- Std_Error <- NULL
  Rule <- CI_lower <- CI_upper <- CATE <- NULL

  if (cate_method %in% c("poisson", "DRLearner")) {
    # Specify the width of the 95% confidence intervals
    interval_95 <- -stats::qnorm((1-0.95)/2)

    # Plot
    g <- ggplot2::ggplot(data = cate_results) +
         ggplot2::geom_hline(yintercept = 0, color = "dark grey", lty = 2) +
         ggplot2::geom_linerange(
                    ggplot2::aes(x = Predictor,
                                 ymin = Estimate - Std_Error*interval_95,
                                 ymax = Estimate + Std_Error*interval_95),
                    lwd = 1,
                    position = ggplot2::position_dodge(width = 1/2))

    g <- g + ggplot2::geom_pointrange(
                   ggplot2::aes(x = Predictor,
                                y = Estimate,
                                ymin = Estimate - Std_Error*interval_95,
                                ymax = Estimate + Std_Error*interval_95),
                                lwd = 1/2,
                                position = ggplot2::position_dodge(width = 1/2),
                                shape = 21, fill = "WHITE") +
        ggplot2::xlab("Causal Rule") +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(
                   paste("CRE Plot:\nConditional Average Treatment Effects",
                         "Per Causal Rule",
                         "\nwith 95% Confidence Intervals\n\n",
                         "CATE Method: ", cate_method))

  } else if (cate_method %in% c("bart-baggr", "cf-means")) {
    # Plot
    g <- ggplot2::ggplot(data = cate_results) +
         ggplot2::geom_hline(yintercept = 0, color = "dark grey", lty = 2) +
         ggplot2::geom_linerange(ggplot2::aes(x = Rule,
                                           ymin = CI_lower,
                                           ymax = CI_upper),
                              lwd = 1,
                              position = ggplot2::position_dodge(width = 1/2)) +
      ggplot2::geom_pointrange(ggplot2::aes(x = Rule,
                                            y = CATE,
                                            ymin = CI_lower,
                                            ymax = CI_upper),
                               lwd = 1/2,
                               position = ggplot2::position_dodge(width = 1/2),
                               shape = 21, fill = "WHITE") +
      ggplot2::xlab("Causal Rule") +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(paste("CRE Plot:\nConditional Average Treatment Effects",
                             "Per Causal Rule",
                             "\nwith 95% Confidence Intervals\n\n",
                             "CATE Method: ", cate_method))

  } else if (cate_method == "linreg") {
    # Plot
    g <- ggplot2::ggplot(data = cate_results) +
      ggplot2::geom_hline(yintercept = 0, color = "dark grey", lty = 2) +
      ggplot2::geom_linerange(ggplot2::aes(x = Rule,
                                           ymin = CI_lower,
                                           ymax = CI_upper),
                              lwd = 1,
                              position = ggplot2::position_dodge(width = 1/2)) +
      ggplot2::geom_pointrange(ggplot2::aes(x = Rule,
                                            y = Estimate,
                                            ymin = CI_lower,
                                            ymax = CI_upper),
                               lwd = 1/2,
                               position = ggplot2::position_dodge(width = 1/2),
                               shape = 21, fill = "WHITE") +
      ggplot2::xlab("Causal Rule") +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(paste("CRE Plot:\nConditional Average Treatment Effects",
                             "Per Causal Rule",
                             "\nwith 95% Confidence Intervals\n\n",
                             "CATE Method: ", cate_method))

  } else {
    stop("Error: Unrecognized CATE method.")
  }

  return(g)
}

#' @title
#' Extend generic plot functions for cre class
#'
#' @description
#' A wrapper function to extend generic plot functions for cre class.
#'
#' @param x  A cre object.
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' Returns a ggplot2 object, invisibly. This function is called for side effects.
#'
#' @export
#'
plot.cre <- function(x, ...){
  g <- ggplot2::autoplot(x, ...)
  print(g)
  invisible(g)
}
