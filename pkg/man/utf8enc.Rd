\name{utf8enc}
\alias{utf8enc}
\title{Encoding UTF8 characters}
\description{utf8enc encodes UTF8 characters to avoid the Warning "found non-ASCII string(s)"}
\usage{utf8enc(utf8var)}
\arguments{	\item{utf8var}{ UTF-8 variable}}
\value{	logic}
\examples{
x <- '\u00E5'
utf8enc(x)
}
\author{Jose' Gama}
\keyword{programming}
