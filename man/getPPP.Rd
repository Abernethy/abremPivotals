\name{getPPP}
\alias{getPPP}
\alias{getPercentilePlottingPositions}

\title{Determination of percentile plotting positions for linear regression with many optional methods}

\description{
Determination of plotting positions to be used for linear regression of life data analysis is a highly debated topic.  This function implements a wide assortment of options for the Abernethy Reliability Methods project.
}

\usage{
getPPP(x, susp=NULL, interval=NULL, ppos="Benard", aranks="Johnson", ties=NULL)
}

%% TODO: rewrite with the idea that all  data are times. Instead of "data" use (life-)time observation
%% 75% complete

\arguments{
  \item{x}{A vector of complete failure (life-)time observations. Alternatively, a dataframe including a time column of fixed-time entries and an event column signifying failures as '1' and (right censored) suspension data as '0' will be accepted.  If the event-time dataframe is provided any value(s) in susp will be ignored.}
  \item{susp}{An optional vector of suspended (right censored) (life-)time observations.}
  \item{interval}{reserved argument for interval censored (life-)time observations - NOT YET IMPLEMENTED.}
  \item{ppos}{A string defining a probability plotting position method.  Implemented options include \code{"Benard"} (as default), \code{"beta"} for the incomplete beta function, \code{"mean"} also known as Herd-Johnson, \code{"Kaplan-Meier"} with modification for final complete failure,  \code{"Hazen"} or \emph{modified} Kaplan-Meier, and \code{"Blom"}.}
  \item{aranks}{A string defining the method for establishing adjusted ranks when suspended (right censored) (life-)time observations are present. Implemented options include \code{"Johnson"} (as default) and \code{"KMestimator"}.}
  \item{ties}{A string defining a method of eliminating ties, or duplicate time valued data, from plotting. Implemented options include \code{"highest"} (used as 'Inspection Option #1' by Abernethy), \code{"mean"}, \code{"lowest"}, and \code{"sequential"}.}
}

%% Jurgen, July 27 2014: I made all options case insensitive in the code. This should be reflected in the help files.
\details{
An alias for \code{getPPP} is available as \code{getProbabilityPlottingPositions}.

}
\value{
Returns a dataframe with the failure data (as potentially reduced by ties argument), the percentile plotting positions, and adjusted ranks. Depending on argument \code{na.rm}, also the event vector of the data.
}

\references{
  Dr. Robert B. Abernethy, (2008) \emph{The New Weibull Handbook}, Fifth Edition
  
  Leonard C. Johnson (1964) \emph{The Statistical Treatment of Fatigue Experiments}  
}

\examples{
fail           <- c(90,96,30,49,82)
susp           <- c(100,45,10)
plot_positions <- getPPP(fail, susp)
median_ranks   <- plot_positions['ppp']

# median ranks based on just an event vector
event<-c(1,0,1,1,0,0,1,1)
pseudo_data <-data.frame(time=seq(1:length(event)), event)
median_ranks2 <- getPPP(pseudo_data)['ppp']
}

\keyword{ regression reliability median ranks }