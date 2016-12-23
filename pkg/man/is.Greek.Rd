\name{is.Greek}
\alias{is.Greek}
\title{Determining if a UTF-8 string is within the Greek Unicode ranges}
\description{is.Greek returns TRUE if a UTF-8 string is within the Greek Unicode ranges}
\usage{is.Greek(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
fileIn<-system.file('extdata/TextGreek.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the Greek Unicode ranges
lapply(unlist(strsplit(x,'')), is.Greek)
}
\author{Jose' Gama}
\keyword{programming}
