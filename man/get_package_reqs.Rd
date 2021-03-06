% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_package_req.R
\name{get_package_reqs}
\alias{get_package_reqs}
\title{Find system requirements}
\usage{
get_package_reqs(package_name, verbose = FALSE)
}
\arguments{
\item{package_name}{\verb{[character]} Name of the package(s)}

\item{verbose}{\verb{[bool]} Print information along the way}
}
\value{
A \code{data.frame} with columns \code{Package} and \code{SystemRequirements} showing all dependencies
with system requirements.
}
\description{
Find all system requirements of a package and all of its dependencies.
}
