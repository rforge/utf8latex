\name{is.Hebrew}
\alias{is.Hebrew}
\title{Determining if a UTF-8 string is within the Hebrew Unicode ranges}
\description{is.Hebrew returns TRUE if a UTF-8 string is within the Hebrew Unicode ranges}
\usage{is.Hebrew(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\author{Jose' Gama}
\examples{
fileIn<-system.file('extdata/TextHebrew.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the Hebrew Unicode ranges
lapply(unlist(strsplit(x,'')), is.Hebrew)
}
\keyword{programming}
