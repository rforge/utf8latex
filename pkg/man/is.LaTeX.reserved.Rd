\name{is.LaTeX.reserved}
\alias{is.LaTeX.reserved}
\title{Determining if a UTF-8 string is within the LaTeX reserved Unicode ranges}
\description{is.LaTeX.reserved returns TRUE if a UTF-8 string is within the LaTeX reserved Unicode ranges}
\usage{is.LaTeX.reserved(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
x <- '^'
# check if every character is within the Katakana Unicode ranges
lapply(unlist(strsplit(x,'')), is.LaTeX.reserved)
}
\author{Jose' Gama}
\keyword{programming}
