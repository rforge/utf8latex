\name{is.cntrl}
\alias{is.cntrl}
\title{Determining if a UTF-8 character is a control code}
\description{is.cntrl returns TRUE if a UTF-8 character is a control code
wrapper function for iscntrl from ctype.h}
\usage{is.cntrl(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
is.cntrl('a')
}
\author{Jose' Gama}
\keyword{programming}
