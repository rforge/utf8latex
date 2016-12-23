\name{is.Katakana}
\alias{is.Katakana}
\title{Determining if a UTF-8 string is within the Katakana Unicode ranges}
\description{is.Katakana returns TRUE if a UTF-8 string is within the Katakana Unicode ranges}
\usage{is.Katakana(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
fileIn<-system.file('extdata/TextKatakana.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the Katakana Unicode ranges
lapply(unlist(strsplit(x,'')), is.Katakana)
}
\author{Jose' Gama}
\keyword{programming}
