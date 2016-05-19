#' Run an expression from X down to N
#'
#' @export
#' @examples
#' downto(3, 5, print("fart"))
#' downto(3, 5, )
#' downto(2, 4, head(mtcars))
#' downto(10, 13, rnorm(5))
#' 10 %>% downto(15, rnorm(5))
#'
#' let <- letters[1:10]
#' downto(1, 6, print(let))
downto <- function(min, max, ...) {
  UseMethod("downto")
}

#' @export
downto.numeric <- function(min, max, ...) {
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
