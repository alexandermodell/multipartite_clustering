clusters_filepath <- "./output/clusters.csv"
degrees_filepath <- "./output/degrees.csv"
common_names_filepath <- "./data/common_names.csv"
drug_categories_filepath <- "./data/drug_categories.csv"
pathway_categories_filepath <- "./data/pathway_categories.csv"

## read common_names, degrees, label and cluster data
common_names <- readr::read_csv(common_names_filepath, show_col_types = FALSE)
degrees <- readr::read_csv(degrees_filepath, show_col_types = FALSE)
clusters <- readr::read_csv(clusters_filepath, show_col_types = FALSE)
drug_categories <- readr::read_csv(drug_categories_filepath, show_col_types = FALSE)
pathway_categories <- readr::read_csv(pathway_categories_filepath, show_col_types = FALSE)

## merge common_names and degrees
clusters <- merge(clusters, common_names)
clusters <- merge(clusters, degrees)
drug_categories <- merge(drug_categories, common_names)
pathway_categories <- merge(pathway_categories, common_names)

## functions to get labels
get_drug_categories <- function(type_i) {
  merge(drug_categories[drug_categories$name %in% type_i$name,], type_i) 
}
get_pathway_categories <- function(type_i) {
  merge(pathway_categories[pathway_categories$name %in% type_i$name,], type_i) 
}
get_cluster <- function(i, type) {
  type_i <- clusters[(clusters$type == type) & (clusters$cluster == i), ]
  type_i[order(type_i$degree, decreasing = TRUE),]
}
get_cluster_labels <- function(i, type) {
  type_i <- get_cluster(i, type)
  if (type == 'Drug') {
    get_drug_categories(type_i)[,c('common_name', 'category')]
  } else if (type == 'Pathway') {
    get_pathway_categories(type_i)[,c('common_name', 'category')]
  } else {
    stop('Categories only available for Drug and Pathway types.')
  }
}

## explore clusters : change cluster index and type to explore
get_cluster(1, 'Drug')
get_cluster_labels(1, 'Drug')
