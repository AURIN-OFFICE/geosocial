% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{WriteStata}
\alias{WriteStata}
\title{WriteStata}
\usage{
WriteStata(DataJoined, SurveyResponses, waves, path)
}
\arguments{
\item{DataJoined}{List which contains the output after the data linkage.}

\item{waves}{vector of years. (For example, wave = [2012,2013,2014])}

\item{path}{Location where the files are going to be written.}

\item{SurveyResponses:}{Dataframe which contains the survey responses.}
}
\value{
True if the folders are created correctly.
}
\description{
Write the outcome of the data linkage.
}
