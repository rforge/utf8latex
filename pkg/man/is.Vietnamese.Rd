\name{is.Vietnamese}
\alias{is.Vietnamese}
\title{Determining if a UTF-8 string is within the Vietnamese Unicode ranges}
\description{is.Vietnamese returns TRUE if a UTF-8 string is within 
the Vietnamese Unicode ranges}
\usage{is.Vietnamese(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
fileIn<-system.file('extdata/TextVietnamese.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the Vietnamese Unicode ranges
lapply(unlist(strsplit(x,'')), is.Vietnamese)
}
\author{Jose' Gama}
\keyword{programming}
