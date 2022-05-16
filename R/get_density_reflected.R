# function to get reflected density of change points
get_density_reflected = function(x, ...) {
  age_reflected <-
    c(-(x$age_rs), x$age_rs, (2-(x$age_rs)))
  d <- 
    stats::density(
      x = age_reflected,
      from = 0,
      to = 1,
      kernel = "gaussian",
      ...)
  
  d <- tibble::tibble(
    x = d$x,
    y = 3 * d$y) %>%
  return()
}
