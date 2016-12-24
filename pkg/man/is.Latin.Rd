\name{is.Latin}
\alias{is.Latin}
\title{Determining if a UTF-8 string is within the Latin Unicode ranges}
\description{is.Latin returns TRUE if a UTF-8 string is within the Latin Unicode ranges}
\usage{is.Latin(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
x <- 'abcd'
# check if every character is within the Latin Unicode ranges
lapply(unlist(strsplit(x,'')), is.Latin)
}
\author{Jose' Gama}
\keyword{programming}
