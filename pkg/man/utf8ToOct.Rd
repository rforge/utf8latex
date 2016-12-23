\name{utf8ToOct}
\alias{utf8ToOct}
\title{Converting from UTF-8 to Octal}
\description{utf8ToOct converts from UTF-8 encoded character to Octal}
\usage{utf8ToOct(utf8char,tZeros=TRUE, sLen=5)}
\arguments{	\item{utf8char}{ UTF-8 string}
	\item{tZeros}{ logic, pad with zero}
	\item{sLen}{ integer, number of digits to return}}
\value{	character}
\examples{
x <- '\u00E4'
utf8ToOct(x)
}
\author{Jose' Gama}
\keyword{programming}
