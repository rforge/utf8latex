\name{is.ChineseTrad}
\alias{is.ChineseTrad}
\title{Determining if the input is UTF-8 Chinese traditional}
\description{is.ChineseTrad returns TRUE if the input is UTF-8 Chinese traditional}
\usage{is.ChineseTrad(utf8char)}
\arguments{	\item{utf8char}{ UTF-8 string}}
\value{	logic}
\source{
Mark Baker
}
\references{
Mark Baker 2010, Recognizing text as Simplified vs. Traditional Chinese
\url{http://stackoverflow.com/questions/4083038/recognizing-text-as-simplified-vs-traditional-chinese}
}
\author{Jose Gama}
\examples{
fileIn<-system.file('extdata/TextChineseTrad.txt', package='utf8latex')
# read a UTF-8 string from a file
x <- scan(file=fileIn,'character', fileEncoding='UTF-8')
# check if every character is within the Chinese Traditional Unicode ranges
lapply(unlist(strsplit(x,'')), is.ChineseTrad)
}
\keyword{programming}
