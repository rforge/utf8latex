\name{u2tex}
\alias{u2tex}
\title{Convert a string or file from UTF-8 to LaTeX with language grouping}
\description{u2tex creates a LaTeX file with language grouping}
\usage{u2tex(inputString=NA, inputFile=NA, outputFile=NA, keepUTF8=TRUE)}
\arguments{	\item{inputString}{ UTF-8 input string}
	\item{inputFile}{ UTF-8 input file}
	\item{outputFile}{ UTF-8 output file}
	\item{keepUTF8}{ logical, if TRUE then the grouped characters are kept in UTF-8 format, if FALSE they are converted to LaTeX}}
\value{	none}
\examples{
\dontrun{
# List of European cities from Wikipedia
fileIn<-system.file('extdata/EuropeanCities.txt', package='utf8latex')
u2tex( inputFile=fileIn, outputFile= 'EuropeanCities.tex', keepUTF8=TRUE)
}
}
\author{Jose' Gama}
\keyword{programming}
