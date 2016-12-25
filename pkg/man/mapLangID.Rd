\name{mapLangID}
\alias{mapLangID}
\title{Determining each character mapped to an identifier for several languages}
\description{mapLangID returns a file with each character mapped to an identifier for several languages
African		'F'
Thai		'T'
Vietnamese	'V'
Cyrillic	'R'
Greek		'G'
Hebrew		'H'
Arabic		'A'
Armenian	'N'
Georgian	'E'
Latin		'L'
}
\usage{mapLangID(langString=NA, langFile=NA, mapFile=NA)}
\arguments{	\item{langString}{ UTF-8 string}
	\item{langFile}{ UTF-8 file}
	\item{mapFile}{ UTF-8 map file}}
\value{	character}
\examples{
\dontrun{
fileIn<-system.file('extdata/TextCyrillic.txt', package='utf8latex')
inputString <- scan(file=fileIn,'character', fileEncoding='UTF-8')
mapLangID(langString=inputString)
}
}
\author{Jose' Gama}
\keyword{programming}
