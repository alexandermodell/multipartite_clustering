setwd("~/Research/Multipartite/biomedical_multipartite_clustering/")

source('./r/main.R')
require(readr)

biomedical_data_filepath <- "./data/network.nt"
clusters_filepath <- "./output/clusters.csv"
degrees_filepath <- "./output/degrees.csv"
min_degree <- 5

## read and preprocess the data
data <- read_and_preprocess_data(biomedical_data_filepath)

## construct the multipartite graph
G <- construct_multipartite_graph(data)

## form regularised Laplacian matrix
L <- to_laplacian(G$A, regulariser = 'auto')

## load the first 1000 singular values of L
load("./data/svs_1000.RData")

## select ambient dimension with elbow method (214)
D <- igraph::dim_select(svs1000)

## load the 214-dimensional truncated SVD of L
load("./data/svd_214.RData")

## or compute with:
# e <- irlba::irlba(L, 1000)

## count positive and negative eigenvalues
Ipq <- sign(colSums(e$u * e$v))[1:D]
p <- sum(Ipq == 1); q <- sum(Ipq == -1); min_pq <- min(p,q)

## construct ambient embedding
X <- e$u[,1:D] %*% diag(sqrt(e$d[1:D]))
row.names(X) <- G$nodes$name

## group by node type
Xs <- lapply(G$types, function(p) X[G$nodes$type == p,]); names(Xs) <- G$types

## compute intrinsic embeddings, selecting the intrinsic dimensions with the elbow method
eXs <- lapply(G$types, function(p) svd(Xs[[p]])); names(eXs) <- G$types
ds <- sapply(eXs, function(e) igraph::dim_select(e$d))
names(ds) <- G$types
Ys <- lapply(G$types, function(ty) Xs[[ty]] %*% eXs[[ty]]$v[,1:ds[[ty]]])
names(Ys) <- G$types

## project embedding onto unit sphere
Yns <- lapply(Ys, wordspace::normalize.rows)

## cluster embeddings of nodes with degree 5 or more
cl <- list()
for (ty in G$types) {
  cl[[ty]] <- kmeans(Yns[[ty]][G$nodes$degree[G$nodes$type == ty] >= min_degree, ], ncol(Yns[[ty]]))$cluster
}

## organise into dataframe and save
cl_df <- stack(cl)
cl_df$name <- rownames(cl_df)
colnames(cl_df) <- c("cluster", "type", "name")
cl_df <- cl_df[,c('name', 'type', 'cluster')]
clusters <- dplyr::arrange(cl_df, type, cluster)
readr::write_csv(clusters, clusters_filepath)

## save node degrees
degrees <- data.frame("name" = G$nodes$name, "degree" = G$nodes$degree)
readr::write_csv(degrees, degrees_filepath)

## scree plot figures
library(latex2exp)

pdf('./plots/ambient_scree_plot.pdf', 5.5, 4.7)
par(mar=c(2.5, 4.1, 2.3, 2.1))
plot(e$d,cex=1.2, xlab='', ylab='singular value', cex.lab = 1.2)
title(' ', cex.main = 1.3)
abline(v=D, lty=1)
legend('topright',
       legend = c(
         TeX(sprintf(r'($\hat{D} = %d$)', D)),
         TeX(sprintf(r'($\hat{p} = %d$)', p)),
         TeX(sprintf(r'($\hat{q} = %d$)', q))
        ),
       bty='n',
       cex=1.3,
       y.intersp=1.3)
dev.off()

pdf("./plots/intrinsic_scree_plots.pdf", 2*3, 1.5*2)
par(mfrow=c(2,3),
    mar=c(2.5, 4.1, 2.1, 0.3))
for (ty in names(eXs)) {
  if (ty %in% c('Drug', 'Variant-Location')) {
    plot(eXs[[ty]]$d, xlab='', ylab='singular value', ylim=c(0,0.8))
  } else {
    plot(eXs[[ty]]$d, xlab='', ylab='', ylim=c(0,0.85), yaxt='n')
  }
  legend('topright',
       legend = TeX(sprintf(r'($\hat{d}_{%d} = %d$)', which(names(eXs)==ty), ds[[ty]])),
       bty='n',
       cex=1.3)
  title(ty)
  abline(v=ds[[ty]], lty=1)
  abline(v=min(p,q), col='black', lty=2)
}
dev.off()


