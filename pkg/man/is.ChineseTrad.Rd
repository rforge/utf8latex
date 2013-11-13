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
Mark Baker 2010, Recognizing text as Simplified vs. Traditional Chinese, http://stackoverflow.com/questions/4083038/recognizing-text-as-simplified-vs-traditional-chinese
}
\author{Jose Gama}
\examples{
dontrun{
is.ChineseTrad('阿尔汉格尔斯克')
is.ChineseTrad('聲音 鳥 樹葉 話 説話 細 又 輕 蝴蝶 請 只有 和 得 聼得到 蜜蜂')
is.ChineseTrad('正體字/繁體字')
is.ChineseTrad('繁体字')
}
}

\keyword{programming}
