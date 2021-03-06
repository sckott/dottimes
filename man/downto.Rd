% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downto.R
\name{downto}
\alias{downto}
\title{Run an expression from X down to N}
\usage{
downto(min, max, ...)
}
\description{
Run an expression from X down to N
}
\examples{
downto(3, 5, print("fart"))
downto(3, 5, )
downto(2, 4, head(mtcars))
downto(10, 13, rnorm(5))
10 \%>\% downto(15, rnorm(5))

let <- letters[1:10]
downto(1, 6, print(let))
}

