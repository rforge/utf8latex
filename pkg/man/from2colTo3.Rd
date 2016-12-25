\name{from2colTo3}
\alias{from2colTo3}
\title{ Create a file with tag data from two to three colums}
\description{from2colTo3 reads a file with 2 columns (hex, tag) and saves it as 3 columns (hex, utf8, tag) }
\usage{from2colTo3(file2cols, file3cols)}
\arguments{
\item{file2cols}{ UTF-8 file with tags in two columns}
\item{file3cols}{ UTF-8 file with tags in three columns}}
\value{	character}
\examples{
\dontrun{
x <- matrix( c('000BC', '\\frac14','000BD','\\frac12','000BE','\\frac34'),3,2,byrow=FALSE)
write(x, file='2cols.tab',ncolumns =2, sep ='\t')
from2colTo3('2cols.tab', '3cols.tab')
read.table('3cols.tab')
}
}
\author{Jose' Gama}
\keyword{programming}
