\name{utf8ToHex}
\alias{utf8ToHex}
\title{Converting from UTF-8 to hexadecimal}
\description{utf8ToHex converts from UTF-8 encoded character to hexadecimal}
\usage{utf8ToHex(utf8char,tZeros=TRUE, sLen=5)}
\arguments{	\item{utf8char}{ UTF-8 string}
	\item{tZeros}{ logic, pad with zero}
	\item{sLen}{ integer, number of digits to return}}
\value{	character}
\examples{
x <- '\u00E4'
utf8ToHex(x)
}
\author{Jose' Gama}
\keyword{programming}
