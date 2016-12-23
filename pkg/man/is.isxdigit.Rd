\name{is.xdigit}
\alias{is.xdigit}
\title{Determining if a UTF-8 character is an hexadecimal digit}
\description{is.xdigit returns TRUE if a UTF-8 character is an hexadecimal digit
wrapper function for isxdigit from ctype.h}
\usage{is.xdigit(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
is.xdigit('7')
}
\author{Jose' Gama}
\keyword{programming}
