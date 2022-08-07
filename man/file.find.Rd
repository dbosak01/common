% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{file.find}
\alias{file.find}
\title{Search for files}
\usage{
file.find(location, file_name, up = 3, down = 3)
}
\arguments{
\item{location}{The directory to start searching from.  The path of the
current program can be retrieved using \code{\link{Sys.path}}.}

\item{file_name}{A full or partial name of the file to find.  If partial,
use the asterisk (*) character to indicate the missing piece.}

\item{up}{The number of levels above the location to search.}

\item{down}{The number of levels below the location to search.}
}
\value{
A vector of one or more full file paths that met the search criteria,
or a NULL if no files met the criteria.
}
\description{
A function to find files on the file system. The function
starts from the directory specified in the \code{location} parameter,
and searches for the file name in the \code{file_name} parameter.
The \code{file_name} accepts wild cards (*).  The function searches
both above and below the current directory locations.  The number of levels
to search can be controlled by the \code{up} and \code{down} parameters.
}
\examples{
# Search for a file named "globals.R"
res <- file.find(Sys.path(), "globals.R")

# Search for SAS datasets
res <- file.find(Sys.path(), "*.sas7bdat")
}