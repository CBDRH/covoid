% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1-seir-cm-vaccine-model.R
\name{seir_cv_model}
\alias{seir_cv_model}
\title{SEIR with heterogeneous contact differential equations and vaccination}
\usage{
seir_cv_model(t, y, parms)
}
\arguments{
\item{t}{Number of time steps over which to sample from the model}

\item{state_t0}{Initial state (S,E,I,R,Sv,Ev,Iv,Rv)}

\item{param}{The model parameters (R0,sigma,gamma,cm,dist,intervention)}
}
\value{
derivatives of SEIR model states with respect to time
}
\description{
SEIR with heterogeneous contact differential equations and vaccination
}
