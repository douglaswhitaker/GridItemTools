% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GridProcessing.R
\name{delete_empty_grid}
\alias{delete_empty_grid}
\title{Remove Empty Grids}
\usage{
delete_empty_grid(resp_list, remove = TRUE)
}
\arguments{
\item{resp_list}{list of matrices.}

\item{remove}{logical. If \code{TRUE} empty matrices are removed from the list.}
}
\value{
A list of grids with empty grids removed or replaced by \code{NA}.
}
\description{
Deletes empty grids from a list, or replaces them with \code{NA}.
}
