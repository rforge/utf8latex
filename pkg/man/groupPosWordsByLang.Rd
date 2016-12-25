\name{groupPosWordsByLang}
\alias{groupPosWordsByLang}
\title{Determining the fontenc and language}
\description{groupPosWordsByLang returns the fontenc and language}
\usage{groupPosWordsByLang(gLangString=NA, wTags=FALSE, vTags=NA, vEndTags=NA)}
\arguments{	\item{gLangString}{ UTF-8 string}
	\item{wTags}{ logic, use default tags}
	\item{vTags}{ vector of custom tags - start tag or standalone tag}
	\item{vEndTags}{ vector of custom tags - end tag}}
\value{	character}
\examples{
\dontrun{
fileIn<-system.file('extdata/TextCyrillic.txt', package='utf8latex')
inputString <- scan(file=fileIn,'character', fileEncoding='UTF-8')
z<-mapLangID(langString=inputString)
z1<-groupWordsByLang(z)
groupPosWordsByLang(z1,TRUE)
}
}
\author{Jose' Gama}
\keyword{programming}
