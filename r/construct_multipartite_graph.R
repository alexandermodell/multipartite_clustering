require(Matrix)

construct_multipartite_graph <- function(edge_list) {
  types <- unique(c(edge_list$P1, edge_list$P2))
  
  tmp1 <- edge_list[,c('V1', 'P1')]; colnames(tmp1) <- c('V', 'P')
  tmp2 <- edge_list[,c('V2', 'P2')]; colnames(tmp2) <- c('V', 'P')
  tmp <- rbind(tmp1, tmp2)
  nodes_df <- unique(tmp)
  ids <- 1:length(nodes_df$V); names(ids) <- nodes_df$V
  nodes <- data.frame('id' = ids, 'name' = nodes_df$V, 'type' = nodes_df$P)
  n <- length(nodes$name)
  ns <- sapply(types, function(p) sum(nodes$type == p))

  edge_list$id1 <- ids[edge_list$V1]
  edge_list$id2 <- ids[edge_list$V2]
  A <- Matrix::sparseMatrix(i = edge_list$id1, j = edge_list$id2, symmetric = TRUE, dims = c(n, n))
  G <- list('A' = A, 'nodes' = nodes, 'types' = types)
  degrees <- rowSums(G$A); names(degrees) <- G$nodes$name
  G$nodes$degree <- degrees
  
  return(G)
}
