\name{getProbabilityPlottingPositions}
\alias{getProbabilityPlottingPositions}
\alias{getPPP}

\title{Determination of probability plotting positions for linear regression with many optional methods}

\description{
Determination of plotting positions to be used for linear regression of life data analysis is a highly debated topic.  This function implements a wide assortment of options for the Abernethy Reliability Methods project.
}

\usage{
getPPP(x, s=NULL, interval=NULL, ppos="Benard", aranks="Johnson", ties=NULL) 
}

%% TODO: rewrite with the idea that all  data are times. Instead of "data" use (life-)time observation
%% 75% complete

\arguments{
  \item{x}{A vector of complete failure (life-)time observations.}
  \item{s}{An optional vector of suspended (right censored) (life-)time observations.}
  \item{interval}{reserved argument for interval censored (life-)time observations - NOT YET IMPLEMENTED.}
  \item{ppos}{A string defining a probability plotting position method.  Implemented options include "Benard" (as default), "beta" for the incomplete beta function, "mean" also known as Herd-Johnson, "Kaplan-Meier" with modification for final complete failure,  "Hazen" or \emph{modified} Kaplan-Meier, and "Blom".}
  \item{aranks}{A string defining the method for establishing adjusted ranks when suspended (right censored) (life-)time observations are present. Implemented options include "Johnson" (as default) and "KMestimator".}
  \item{ties}{A string defining a method of eliminating ties, or duplicate time valued data, from plotting. Implemented options include "highest" (used as 'Inspection Option #1' by Abernethy), "mean", "lowest", and "sequential".}
}

%% Jurgen, July 27 2014: I made all options case insensitive in the code. This should be reflected in the help files.
\details{
An alias for \code{getProbabilityPlottingPositions} is available as \code{getPPP}.
}
\value{
Returns a dataframe with the failure data (as potentially reduced by ties argument), the probability plotting positions, and adjusted ranks.
%% TODO: "probability plotting positions" is according to Jurgen misused here -> discuss
}

\references{
  Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
  Leonard C. Johnson (1964) "The Statistical Treatment of Fatique Experiments"  
}

\examples{
fail <- c(90,96,30,49,82)
susp <- c(100,45,10)
print(plot_positions <- getPPP(fail, susp))
print(median_ranks <- plot_positions[,2])
}

\keyword{ regression reliability }