% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_rules.R
\name{parse_rules}
\alias{parse_rules}
\title{Parse rules in JSON files}
\usage{
parse_rules(system_req_folder)
}
\arguments{
\item{system_req_folder}{The location of a local clone of "System Requirements for R Packages".}
}
\value{
A \code{data.frame} with rules info in these columns:
\itemize{
\item "Rule": Name of the rule/JSON file.
\item "Packages": Name of the system package.
\item "OS": The operating system (Linux or Windows).
\item "Distribution": The Linux distribution. \code{NA} if \code{OS} is "Windows".
\item "Patterns": Regular expression pattern to match \code{Package}.
\item "CollapsedPatterns": If there are multiple entries in \code{Patterns}, they are here collapsed into
a single pattern to work with \code{\link[=grepl]{grepl()}}.
}
}
\description{
Parse system requirements in JSON format.
}
\seealso{
distro_req
}
