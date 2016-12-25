\name{utf8texc}
\alias{utf8texc}
\title{Converting a string from UTF-8 to LaTeX}
\description{utf8texc converts a string from UTF-8 to LaTeX}
\usage{utf8texc(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	LaTeX string}
\author{Jose' Gama}
\examples{
\dontrun{
fileIn<-system.file('extdata/TextHiragana.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
utf8texc(x)
}
}
\keyword{programming}
