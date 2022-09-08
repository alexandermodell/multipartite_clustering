# Clustering of a multipartite biomedical network

This repositiory contains code to reproduce and further explore the clustering of the multipartite biomedical network described in the paper 'Spectral embedding and the latent geometry of multipartite networks'.

The `clustering.R` file contains code to compute the clustering and reproduce the scree plots in Fig. 5. To run this code, the raw data must be downloaded, uncompressed and placed into the `data` folder. The data can be downloaded here: https://github.com/zongnansu1982/Prediction_Basedon_Biolinkednetwork_Mining/blob/master/data/network.zip?raw=true. The 1000-dimensional SVD has been precomputed, however only the first 214 singular vectors required for the embedding are retained in the repository. Different ambient embedding dimensions may be tried by modifying the code and recomputing the SVD.

The `explore_clusters.R` file contains code to explore the clusters and examine the relevant node labels in a user-friendly way. 