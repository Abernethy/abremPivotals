\name{getPPP}
\alias{getPPP}
\alias{getPercentilePlottingPositions}

\title{Determination of percentile plotting positions for linear regression with many optional methods}

\description{
Determination of plotting positions to be used for linear regression of life data analysis is a highly debated topic.  This function implements a wide assortment of options for the Abernethy Reliability Methods project.
}

\usage{
getPPP(x, susp=NULL, interval=NULL, ppos="Benard", aranks="Johnson", ties=NULL,
 na.rm=TRUE)
}

%% TODO: rewrite with the idea that all  data are times. Instead of "data" use (life-)time observation
%% 75% complete

\arguments{
  \item{x}{A vector of complete failure (life-)time observations.}
  \item{susp}{An optional vector of suspended (right censored) (life-)time observations.}
  \item{interval}{reserved argument for interval censored (life-)time observations - NOT YET IMPLEMENTED.}
  \item{ppos}{A string defining a probability plotting position method.  Implemented options include \code{"Benard"} (as default), \code{"beta"} for the incomplete beta function, \code{"mean"} also known as Herd-Johnson, \code{"Kaplan-Meier"} with modification for final complete failure,  \code{"Hazen"} or \emph{modified} Kaplan-Meier, and \code{"Blom"}.}
  \item{aranks}{A string defining the method for establishing adjusted ranks when suspended (right censored) (life-)time observations are present. Implemented options include \code{"Johnson"} (as default) and \code{"KMestimator"}.}
  \item{ties}{A string defining a method of eliminating ties, or duplicate time valued data, from plotting. Implemented options include \code{"highest"} (used as 'Inspection Option #1' by Abernethy), \code{"mean"}, \code{"lowest"}, and \code{"sequential"}.}
  \item{na.rm}{Logical value controlling whether the return dataframe might contain \code{NA} values in its  \code{$ppp} columns. This argument also controls the inclusion of an \code{$event} column in the value dataframe.}
}

%% Jurgen, July 27 2014: I made all options case insensitive in the code. This should be reflected in the help files.
\details{
An alias for \code{getPPP} is available as \code{getProbabilityPlottingPositions}.

The \code{na.rm} argument allows for two types of return dataframes: When \code{na.rm = TRUE} (the default), the function returns a dataframe containing only percentile plotting positions of the failures (corresponding with event = 1 or 'dead'). In that case it makes no sense to include the event vector in the dataframe, so it is omitted (all event indicators are \code{1}).%Note that when suspended data are passed using the \code{susp} argument,  
When \code{na.rm = FALSE}, the function returns a dataframe containing \emph{all} supplied time data and calculated percentile plotting positions. In the case of suspended data, the real percentile plotting position is undefined so it is represented by \code{NA}. The corresponding event vector detailing the censoring scheme is returned as an extra column in the return dataframe.
}
\value{
Returns a dataframe with the failure data (as potentially reduced by ties argument), the percentile plotting positions, and adjusted ranks. Depending on argument \code{na.rm}, also the event vector of the data.
}

\references{
  Dr. Robert B. Abernethy, (2008) \emph{The New Weibull Handbook}, Fifth Edition
  
  Leonard C. Johnson (1964) \emph{The Statistical Treatment of Fatique Experiments}  
}

\examples{
fail           <- c(90,96,30,49,82)
susp           <- c(100,45,10)
plot_positions <- getPPP(fail, susp)
median_ranks   <- plot_positions['ppp']

median_ranks2 <- getPPP(c(1,0,1,1,0,1,0,0,1,1))['ppp']
    # median ranks based on just an event vector

}

\keyword{ regression reliability median ranks }