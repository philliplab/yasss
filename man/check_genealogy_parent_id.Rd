% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genealogy.R
\name{check_genealogy_parent_id}
\alias{check_genealogy_parent_id}
\title{Check the parent_id column in a genealogy}
\usage{
check_genealogy_parent_id(genealogy, result = list())
}
\arguments{
\item{genealogy}{The genealogy to check.}

\item{result}{The list to which the result will be added and from which
previous result will be drawn to check the prerequisites.}
}
\value{
A list with TRUE or FALSE indicating whether the related check
passed.
}
\description{
Checks that parent_id is not missing, is a valid integer and that it
references a parent that exists.
}
