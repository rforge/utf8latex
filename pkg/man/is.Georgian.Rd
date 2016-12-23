\name{is.Georgian}
\alias{is.Georgian}
\title{Determining if a UTF-8 string is within the Georgian Unicode ranges}
\description{is.Georgian returns TRUE if a UTF-8 string is within the Georgian Unicode ranges}
\usage{is.Georgian(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\author{Jose' Gama}
\examples{
fileIn<-system.file('extdata/TextGeorgian.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the Georgian Unicode ranges
lapply(unlist(strsplit(x,'')), is.Georgian)
}
\keyword{programming}
