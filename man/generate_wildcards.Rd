% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_data.R
\name{generate_wildcards}
\alias{generate_wildcards}
\title{Generate wildcard datasets given a template.}
\usage{
generate_wildcards(df.fct, soln.fct, replicates = 50, cols = NULL)
}
\arguments{
\item{df.fct}{Function which generates a single dataset that could be given
to a student. This function should not take any parameters.}

\item{soln.fct}{Function that computes additional wildcards from the dataset,
which are often the solutions to calculated questions. This function should
take a single argument (the resulting call of \code{df.fct}) and return a
named vector of computed values.}

\item{replicates}{The number of datasets to generate (max is 50).}

\item{cols}{columns in the result of \code{df.fct} that should not be
considered wildcards. Takes any form of \code{dplyr::tidyselect}.}
}
\value{
a dataframe that contains all the wildcards and their values.
}
\description{
Wildcards hold the various problems in Moodle. When you want each student
to have a unique dataset, there are many wildcards to generate. This
provides a way of generating the wildcards given a template of how to
construct an individual dataset.
}
