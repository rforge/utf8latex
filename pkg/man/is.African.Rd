\name{is.African}
\alias{is.African}
\title{Determining if a UTF-8 string is within the African Unicode ranges}
\description{is.African returns TRUE if a UTF-8 string is within the African Unicode ranges}
\usage{is.African(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
fileIn<-system.file('extdata/TextAfrican.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the African Unicode ranges
lapply(unlist(strsplit(x,'')), is.African)
}
\author{Jose' Gama}
\keyword{programming}
