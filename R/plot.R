#' @title
#' A helper function for CRE object
#'
#' @description
#' A helper function to plot CRE object using ggplot2 package.
#'
#' @param object A CRE object.
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' Returns a ggplot object.
#'
#' @keywords internal
#' @importFrom ggplot2 autoplot
#'
autoplot.cre <- function(object, ...) {

  "%>%" <- magrittr::"%>%"

  Rule <- Estimate <- CI_lower <- CI_upper <- NULL

  gg_labs <- gg_title <- NULL

  # collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)
  for (i in arg_names) {
    assign(i, unlist(dot_args[i], use.names = FALSE))
  }

  cate <- object[["CATE"]]
  ate <- cate[1, ]
  aate <- cate[2:nrow(cate), ]
  aate <- aate[order(aate$Estimate, decreasing = TRUE), ]
  rownames(aate) <- seq_len(nrow(aate))

  g <- ggplot2::ggplot(data = aate) +
    ggplot2::geom_hline(yintercept = 0, color = "dark grey", lty = 2) +
    ggplot2::geom_linerange(ggplot2::aes(x = reorder(Rule, Estimate),
                                         ymin = CI_lower,
                                         ymax = CI_upper),
                            lwd = 1,
                            position = ggplot2::position_dodge(width = 1 / 2)) +
    ggplot2::geom_pointrange(ggplot2::aes(x = reorder(Rule, Estimate),
                                          y = Estimate,
                                          ymin = CI_lower,
                                          ymax = CI_upper),
                             lwd = 1 / 2,
                             position = ggplot2::position_dodge(width = 1 / 2),
                             shape = 21, fill = "WHITE") +
    ggplot2::xlab("") +
    ggplot2::ylab("AATE") +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(paste("Causal Rule Ensemble: ",
                           "\nConditional Average Treatment Effect",
                           "\nLinear Decomposition",
                           "\n\nATE = ", round(ate[["Estimate"]], 3),
                           " [", round(ate[["CI_lower"]], 3), ",",
                           round(ate[["CI_upper"]], 3), "]", sep = "")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  return(g)
}

#' @title
#' Extend generic plot functions for CRE class
#'
#' @description
#' A wrapper function to extend generic plot functions for CRE class.
#'
#' @param x  A CRE object.
#' @param ... Additional arguments passed to customize the plot.
#'
#' @return
#' Returns a ggplot2 object, invisibly. This function is called for side
#' effects.
#'
#' @export
#'
plot.cre <- function(x, ...) {
  if (x[["M"]]["select_significant"] == 0) {
    message("Visualization not available (0 causal decision rules discovered).")
  } else {
    g <- ggplot2::autoplot(x, ...)
    print(g)
    invisible(g)
  }
}
