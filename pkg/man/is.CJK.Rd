\name{is.CJK}
\alias{is.CJK}
\title{Determining if a UTF-8 string is within the CJK Unicode ranges}
\description{is.CJK returns TRUE if a UTF-8 string is within the CJK Unicode ranges}
\usage{is.CJK(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
fileIn<-system.file('extdata/TextCJK.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the CJK Unicode ranges
lapply(unlist(strsplit(x,'')), is.CJK)
}
\author{Jose' Gama}
\keyword{programming}
