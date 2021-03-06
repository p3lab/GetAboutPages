% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_about_pages.r
\name{url_exists}
\alias{url_exists}
\title{Check if a url exists with httr}
\usage{
url_exists(url, non_2xx_return_value = FALSE, quiet = TRUE, timeout_thres = 10)
}
\arguments{
\item{url}{A URL to check}

\item{non_2xx_return_value}{what to do if the site exists but the
HTTP status code is not in the \verb{2xx} range. Default is to return \code{TRUE}.}

\item{quiet}{if not \code{FALSE}, then every time the \code{non_2xx_return_value} condition
arises a warning message will be displayed. Default is \code{TRUE}.}

\item{timeout_thres}{timeout in seconds for httr attempt}
}
\value{
A boolean value to indicate whether a website is reachable
}
\description{
Check if a url exists with httr
}
