% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wiki_graph.R
\docType{data}
\name{wiki_graph}
\alias{wiki_graph}
\title{Edges of a graph with their corresponding weights}
\format{
A data frame with 18 rows and 3 variables:
\describe{
  \item{v1}{source node of the edge}
  \item{v2}{destination node of the egde}
  \item{w}{weight of the edge}
}
}
\source{
\url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
}
\usage{
wiki_graph
}
\description{
A dataset containing the source and destination nodes(each node being
represented by a natural number starting from 1) of each edge of a
connected graph along with their weights.
}
\keyword{datasets}
