# function to get normalised density estimates of change points 
get_density_norm <- function(x,...) {
  d <- stats::density(
    x = x$age_rs,
    from = 0,
    to = 1,
    kernel = "gaussian",
    ...)
  
  d <- tibble::tibble(
    x = d$x,
    y = d$y) %>%
    return()
}

