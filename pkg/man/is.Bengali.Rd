\name{is.Bengali}
\alias{is.Bengali}
\alias{is.Brahmi}
\alias{is.Chakma}
\alias{is.Devanagari}
\alias{is.Gujarati}
\alias{is.Kannada}
\alias{is.Tamil}
\alias{is.Telugu}
\title{Determining if a UTF-8 string is within the Bengali Unicode ranges}
\description{is.Bengali returns TRUE if a UTF-8 string is within the Bengali Unicode ranges
	is.Brahmi returns TRUE if a UTF-8 string is within the Brahmi Unicode ranges
	is.Chakma returns TRUE if a UTF-8 string is within the Chakma Unicode ranges
	is.Devanagari returns TRUE if a UTF-8 string is within the Devanagari Unicode ranges
	is.Gujarati returns TRUE if a UTF-8 string is within the Gujarati Unicode ranges
	is.Kannada returns TRUE if a UTF-8 string is within the Kannada Unicode ranges
	is.Tamil returns TRUE if a UTF-8 string is within the Tamil Unicode ranges
	is.Telugu returns TRUE if a UTF-8 string is within the Telugu Unicode ranges
}
\usage{is.Bengali(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\examples{
fileIn<-system.file('extdata/TextBengali.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the Bengali Unicode ranges
lapply(unlist(strsplit(x,'')), is.Bengali)
}
\author{Jose' Gama}
\keyword{programming}
