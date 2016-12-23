\name{is.Arabic}
\alias{is.Arabic}
\title{Determining if a UTF-8 string is within the Arabic Unicode ranges}
\description{is.Arabic returns TRUE if a UTF-8 string is within the Arabic Unicode ranges}
\usage{is.Arabic(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
fileIn<-system.file('extdata/TextArabic.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the Arabic Unicode ranges
lapply(unlist(strsplit(x,'')), is.Arabic)
}
\author{Jose' Gama}
\keyword{programming}
