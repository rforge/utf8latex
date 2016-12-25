\name{utf8tex}
\alias{utf8tex}
\title{Converting a variable from UTF-8 to LaTeX}
\description{utf8tex converts a vector, matrix or dataframe from UTF-8 to LaTeX}
\usage{utf8tex(utf8var)}
\arguments{	\item{utf8var}{ UTF-8 variable}}
\value{	LaTeX variable}
\examples{
\dontrun{
fileIn<-system.file('extdata/TextHiragana.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
utf8tex(x)
}
}
\author{Jose' Gama}
\keyword{programming}
