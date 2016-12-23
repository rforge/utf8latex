\name{is.Hangul}
\alias{is.Hangul}
\title{Determining if a UTF-8 string is within the Hangul Unicode ranges}
\description{is.Hangul returns TRUE if a UTF-8 string is within the Hangul Unicode ranges}
\usage{is.Hangul(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\author{Jose' Gama}
\examples{
fileIn<-system.file('extdata/TextHangul.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the Hangul Unicode ranges
lapply(unlist(strsplit(x,'')), is.Hangul)
}
\keyword{programming}
