% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{parsePDF}
\alias{parsePDF}
\title{Parse the pdf document}
\usage{
parsePDF(xpert, processed_by, config, pool)
}
\arguments{
\item{xpert}{path of pdf OR raw binary (from req)}

\item{processed_by}{character string of who or what is processing this xpert result}

\item{config}{\link{Config} R6 class}

\item{pool}{\link{Pool} database connection pool}
}
\value{
\code{list(results = df)} or \code{list(result = df, msg = "error message")} where \code{df} contains xpert results
written to a database
}
\description{
Will parse xpert results from pdf and store in a database.  This function does not upload to remote sites nor
does it check if the files are pdfs (other than using \link{pdftools} functions)
}
