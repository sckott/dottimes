#' Run an expression from X up to N
#'
#' @export
#' @examples
#' upto(3, 5, print("fart"))
#' upto(3, 5, )
#' upto(2, 4, head(mtcars))
#' upto(10, 13, rnorm(5))
#' 10 %>% upto(15, rnorm(5))
#'
#' let <- letters[1:10]
#' upto(1, 6, print(let))
upto <- function(min, max, ...) {
  UseMethod("upto")
}

#' @export
upto.numeric <- function(min, max, ...) {
  ogmin <- min
  dots <- lazyeval::lazy_dots(...)
  if (length(dots) == 1 && !purrr:::has_names(dots)) {
    dots <- dots[[1]]
  }
  out <- vector("list", length(min:max))
  for (min in min:max) {
    out[[min]] <- lazyeval::lazy_eval(dots)
  }
  out[ogmin:max]
}
