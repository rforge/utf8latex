\name{idEncoding}
\alias{idEncoding}
\title{detect the encoding of a text file}
\description{idEncoding detects the encoding of a text file by checking the byte order mark (BOM)}
\usage{idEncoding(filename)}
\arguments{	\item{filename}{ UTF-8 file}}
\value{	character}
\examples{
\dontrun{
fileIn<-system.file('extdata/TextGreekBom.txt', package='utf8latex')
idEncoding(fileIn)
}
}
\author{Wikipedia}
\keyword{programming}
