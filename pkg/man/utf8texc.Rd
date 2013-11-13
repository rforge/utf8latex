\name{utf8texc}
\alias{utf8texc}
\title{Converting a string from UTF-8 to LaTeX}
\description{utf8texc converts a string from UTF-8 to LaTeX}
\usage{utf8texc(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	LaTeX string}
\author{Jose' Gama}
\examples{
dontrun{
x<-'ÅÅa𝒜'
utf8texc(x)
}
}
\keyword{programming}
