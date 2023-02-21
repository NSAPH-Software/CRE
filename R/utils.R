# Keeping logger options
my_options <- new.env(parent = emptyenv())

get_options <- function(k, v) {
  my_options[[k]]
}

set_options <- function(k, v) {
  my_options[[k]] <- v
}

list_options <- function() {
  names(my_options)
}

g_wc_str <- function(start, end) {
  wc_rd <- (end - start)[[3]]
  wc_string <- paste(sprintf("%.3f", wc_rd), "seconds")
  return(wc_string)
}
