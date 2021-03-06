% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/consensusMatrix.R
\name{consensusMatrix_character}
\alias{consensusMatrix_character}
\title{Computes a consensus matrix.}
\usage{
consensusMatrix_character(x)
}
\arguments{
\item{x}{The sequence data as a set of character vectors}
}
\value{
A matrix with columns representing positions in the sequence and rows the allowed characters in IUPAC notation of nucleotides and cells the counts of the respective character at the respective position.
}
\description{
Tallies characters at each position for a set of character vectors without any Bioconductor packages.
}
\details{
Designed to function like consensusMatrix from Biostrings.

This function can handle upper and lower case input, but will only produce upper case output.

To prevent a dependency on a bioconductor package, this is not properly set
up as a method for the consensusMatrix generic defined in Biostrings.
}
