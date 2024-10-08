% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_preparation.R
\name{create_projection_data}
\alias{create_projection_data}
\title{Prepare data for local projections.}
\usage{
create_projection_data(data_obj, dependent_var, shock_var, lagged_vars,
  proj_horizon = 8, id_vars = c("Country", "Year"))
}
\arguments{
\item{data_obj}{The data to be used for the projections}

\item{dependent_var}{The dependent variable for the estimation}

\item{shock_var}{The shock variable}

\item{lagged_vars}{The variables included as lags on the RHS}

\item{proj_horizon}{The time horizon for the projections, 8 by defauls}

\item{id_vars}{The id vars for the underlying panel data; should be given as variable name or index}
}
\value{
A data frame to be used for local projection estimation
}
\description{
\code{create_projection_data} prepares the data used for computing projections.
}
\details{
\code{create_projection_data} takes the original data and adds lags, dlags and future
changes of the relevant variables. This is the data set on which then projections can
be computed on.
}
