% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moodle_xml_helpers.R
\name{mxml_read_calculated}
\alias{mxml_read_calculated}
\title{Convert HTML output for Calculated Questions into Moodle XML.}
\usage{
mxml_read_calculated(node, opts)
}
\arguments{
\item{node}{the html node corresponding to the calculated
question.}

\item{opts}{named character vector of options associated with this question,
generally the result of a call to \code{\link{mxml_obtain_options}}.}
}
\value{
an XML node to be inserted into a document.
}
\description{
HTML code generated from knitting together an RMarkdown document which
corresponds to a Calculated Question is converted to Moodle XML. This is
called from within \code{\link{html_to_moodle}}.
}
