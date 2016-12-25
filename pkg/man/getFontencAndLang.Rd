\name{getFontencAndLang}
\alias{getFontencAndLang}
\title{Determining the fontenc and language}
\description{getFontencAndLang returns the fontenc and language}
\usage{getFontencAndLang(mLangString=NA, mLangFile=NA)}
\arguments{
\item{mLangString}{ UTF-8 string}
\item{mLangFile}{ UTF-8 file}}
\value{	character}
\examples{
\dontrun{
fileIn<-system.file('extdata/TextCyrillic.txt', package='utf8latex')
inputString <- scan(file=fileIn,'character', fileEncoding='UTF-8')
z<-mapLangID(langString=inputString)
z1<-groupWordsByLang(z)
z2<-groupPosWordsByLang(z1,TRUE)
langFontEnc<-getFontencAndLang(inputString)
}
}
\author{Jose' Gama}
\keyword{programming}
