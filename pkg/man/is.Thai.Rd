\name{is.Thai}
\alias{is.Thai}
\title{Determining if a UTF-8 string is within the Thai Unicode ranges}
\description{is.Thai returns TRUE if a UTF-8 string is within the Thai Unicode ranges}
\usage{is.Thai(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
fileIn<-system.file('extdata/TextThai.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the Thai Unicode ranges
lapply(unlist(strsplit(x,'')), is.Thai)
}
\author{Jose' Gama}
\keyword{programming}
