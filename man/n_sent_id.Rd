% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_sent_id.r
\name{n_sent_id}
\alias{n_sent_id}
\title{Count sent IDs from txt files}
\usage{
n_sent_id(sent_id_path, date_from = NULL, date_to = NULL)
}
\arguments{
\item{sent_id_path}{Directory contains .txt files of sent IDs, 
of which file name contains date.}

\item{date_from}{Date begin.}

\item{date_to}{Date end.}
}
\description{
Count sent IDs from txt files
}
\examples{
n_sent_id("./exclude_id/", date_from = "2015-09-01", date_to = "2015-09-30")
}

