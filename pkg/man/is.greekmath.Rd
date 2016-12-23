\name{is.greekmath}
\alias{is.greekmath}
\title{Determining if a UTF-8 string is within the greekmath Unicode ranges}
\description{is.greekmath returns TRUE if a UTF-8 string is within the greekmath Unicode ranges}
\usage{is.greekmath(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\author{Jose' Gama}
\examples{
fileIn<-system.file('extdata/Textgreekmath.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the greek-math Unicode ranges
lapply(unlist(strsplit(x,'')), is.greekmath)
}
\keyword{programming}
