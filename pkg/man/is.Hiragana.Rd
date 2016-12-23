\name{is.Hiragana}
\alias{is.Hiragana}
\title{Determining if a UTF-8 string is within the Hiragana Unicode ranges}
\description{is.Hiragana returns TRUE if a UTF-8 string is within the Hiragana Unicode ranges}
\usage{is.Hiragana(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\author{Jose' Gama}
\examples{
fileIn<-system.file('extdata/TextHiragana.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the Hiragana Unicode ranges
lapply(unlist(strsplit(x,'')), is.Hiragana)
}
\keyword{programming}
