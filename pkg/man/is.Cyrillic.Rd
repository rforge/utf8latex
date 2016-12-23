\name{is.Cyrillic}
\alias{is.Cyrillic}
\title{Determining if a UTF-8 string is within the Cyrillic Unicode ranges}
\description{is.Cyrillic returns TRUE if a UTF-8 string is within the Cyrillic Unicode ranges}
\usage{is.Cyrillic(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
fileIn<-system.file('extdata/TextCyrillic.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the Cyrillic Unicode ranges
lapply(unlist(strsplit(x,'')), is.Cyrillic)
}
\author{Jose' Gama}
\keyword{programming}
