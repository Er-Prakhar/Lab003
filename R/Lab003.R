#' Lab003: A package containing R implementations of two well known algorithms:
#' Euclidean and Dijkstra
#'
#' This package contains two functions: \code{euclidean} and \code{dijkstra}
#' along with a data set \code{wiki_graph} which is a data frame containing the
#' structure of a graph.
#'
#' @section Lab003 functions:
#'   \code{euclidean} is a function based on the Euclidean algorithm that takes
#'   two non negative integers as input and gives their greatest common divisor
#'   as the output.
#'
#'   \code{dijkstra} is a function based on the Dijkstra algorithm to find the
#'   shortest distances from a given node in a graph to every other node of the
#'   graph.
#'
#' @section Lab003 datasets:
#'  \code{wiki_graph} is a data frame containing the structure of the first
#'  graph on the Wikipedia page for Dijkstra algorithm (
#'  \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}).
#'  This data frame has three variables: \code{v1, v2, w} where each row
#'  represents an edge of the graph from node v1 to node v2, along with the
#'  weight w of the graph
#'
#' @docType package
#'
#' @name Lab003
#'
NULL
