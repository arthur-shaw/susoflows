% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delete.R
\name{delete_in_dir}
\alias{delete_in_dir}
\title{Delete everything inside a target directory}
\usage{
delete_in_dir(dir)
}
\arguments{
\item{dir}{Character. Path to target directory}
}
\value{
Side-effect of deleting stale files.
}
\description{
Delete everything in a target directory:
files, sub-directories, and files in those sub-directories.
}
