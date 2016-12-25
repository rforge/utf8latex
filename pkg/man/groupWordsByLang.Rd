\name{groupWordsByLang}
\alias{groupWordsByLang}
\title{Grouping words by language}
\description{groupWordsByLang groups words by language}
\usage{groupWordsByLang(mLangString=NA, mLangFile=NA)}
\arguments{
	\item{mLangString}{ UTF-8 string}
	\item{mLangFile}{ UTF-8 file}}
\value{	character}
\examples{
\dontrun{
fileIn<-system.file('extdata/TextCyrillic.txt', package='utf8latex')
inputString <- scan(file=fileIn,'character', fileEncoding='UTF-8')
z<-mapLangID(langString=inputString)
groupWordsByLang(z)
}
}
\author{Jose' Gama}
\keyword{programming}
