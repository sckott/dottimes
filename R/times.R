#' Run an expression N times
#'
#' @export
#' @examples
#' times(3, print("fart"))
#' times(3, head(mtcars))
#' times(10, rnorm(5))
#' 10 %>% times(rnorm(5))
times <- function(x, ...) {
  UseMethod("times")
}

#' @export
times.numeric <- function(x, ...) {
  dots <- lazyeval::lazy_dots(...)
  if (length(dots) == 1 && !purrr:::has_names(dots)) {
    dots <- dots[[1]]
  }
  out <- vector("list", x)
  for (i in seq_len(x)) {
    out[[i]] <- lazyeval::lazy_eval(dots)
  }
  out
}
