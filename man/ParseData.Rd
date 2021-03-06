% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataOrganization.R
\name{ParseData}
\alias{ParseData}
\title{Parse Data}
\usage{
ParseData(data, key, parser = OnlyLettersNumbers)
}
\arguments{
\item{data}{A data frame containing a string-encoded response options}

\item{key}{A data frame of the same size and shape as \code{data}, but containing the keyed-correct responses}

\item{parser}{A parser function for the strings. Parsers stars (*), commas (,) ,and spaces( ) by default.}
}
\description{
Parse data into a generic form.
}
\examples{
### Pasrse the data
parsedData <- ParseData(resps,key)

### Code the data based on the keys
codedData <- Code(parsedData)
}
\keyword{code}
