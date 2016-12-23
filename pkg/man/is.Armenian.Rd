\name{is.Armenian}
\alias{is.Armenian}
\title{Determining if a UTF-8 string is within the Armenian Unicode ranges}
\description{is.Armenian returns TRUE if a UTF-8 string is within the Armenian Unicode ranges}
\usage{is.Armenian(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
fileIn<-system.file('extdata/TextArmenian.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the Armenian Unicode ranges
lapply(unlist(strsplit(x,'')), is.Armenian)
}
\author{Jose' Gama}
\keyword{programming}
