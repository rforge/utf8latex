\name{utf8tex}
\alias{utf8tex}
\title{Converting a variable from UTF-8 to LaTeX}
\description{utf8tex converts a vector, matrix or dataframe from UTF-8 to LaTeX}
\usage{utf8tex(utf8var)}
\arguments{	\item{utf8var}{ UTF-8 variable}}
\value{	LaTeX variable}
\author{Jose' Gama}
\examples{
dontrun{
x<-c('Åõ','aéĩ','𝒜')
utf8tex(x)
x<-matrix(c('Åõ','aéĩ','𝒜ĩ','Åõ','aéĩ','𝒜ĩ'),2)
utf8tex(x)
x<-data.frame(x,1:2)
utf8tex(x)
}
}
\keyword{programming}
