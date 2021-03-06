% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/joint_entropy.R
\name{joint_entropy}
\alias{joint_entropy}
\title{Joint Entropies}
\usage{
joint_entropy(dat, dec = 3)
}
\arguments{
\item{dat}{Dataframe with rows as observations and columns as variables. Variables must all be observed or transformed categorical with finite range spaces.}

\item{dec}{The precision given in number of decimals for which
the frequency distribution of unique entropy values is created. Default is 3.}
}
\value{
List with the upper triangular joint entropy matrix (univariate entropy in diagonal) and the frequency distribution of unique joint entropy values,
}
\description{
Calculates the joint entropies between pairwise variables.
}
\details{
To be completed
}
\references{
Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data. *Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique*, 129(1), 45-63.
}
\author{
Termeh Shafie
}
