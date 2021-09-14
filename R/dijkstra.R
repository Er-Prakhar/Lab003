#' Implementation of the Dijkstra algorithm
#' The algorithm takes a graph and an initial node 
#' and calculates the shortest path from the initial 
#' node to every other node in the graph.
#' 
#' Pseudocode from: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#'
#' @param graph Graph with weights
#' @param init_node Starting node
#' @return shortest path to every other node
#' @examples
#' dijkstra(graph, 1)
#' @export

dijkstra <- function(graph, init_node) {
  
  print(is.data.frame(graph))
  
  stopifnot("graph should be a data frame with variables v1, v2, w" =
              is.data.frame(graph) & all(c("v1", "v2", "w") %in% names(graph)) & length(graph) == 3,
            "graph should be numeric" = is.numeric(c(graph$v1, graph$v2, graph$w)),
            "no duplicate edges allowed in the graph" = !any(duplicated(data.frame(graph$v1, graph$v2))),
            "no edge should have the same source and destination" = !any(graph$v1 == graph$v2)
            )
  
  vertices <- as.numeric(levels(factor(c(graph$v1, graph$v2))))
  
  stopifnot("init_node must be a node in graph" = init_node %in% vertices)
  
  dist <- 0
  
  for(i in 1:length(vertices)) {
    
    dist[i] <- -1
    
  }
  
  dist[vertices == init_node] <- 0
  
  for(x in 1:(length(vertices)-1)) {
    
    #set of contenders(indices) for current vertex
    i <- which(vertices != 0 & dist != -1)
    
    cur_in <- i[1]
    
    #selecting the current index from set of contenders
    for(j in i) {
      
      if(dist[cur_in] > dist[j])
        cur_in <- j
      
    }
    
    u <- vertices[cur_in]
    
    vertices[cur_in] <- 0
    
    i <- which(graph$v1 == u)
    
    #selecting the adjacent vertices of the current vertex
    adjacents <- graph$v2[i]
    
    #indices of adjacents in vertices
    ad_indices <- which(vertices %in% adjacents)
    
    #setting the alternate distances if found shorter
    for(j in ad_indices) {
      
      alt <- dist[cur_in] + graph$w[which(graph$v1 == u & graph$v2 == vertices[j])]
      
      if(dist[j] < 0 | alt < dist[j])
        dist[j] <- alt

    }
    
  }   
  
  return(dist)
  
}