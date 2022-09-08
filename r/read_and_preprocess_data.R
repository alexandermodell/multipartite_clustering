require(readr)
require(magrittr)
require(stringr)

read_and_preprocess_data <- function(f) {
  relationships <- data.frame(
    "relationship" = c(
      "<http://bio2rdf.org/MultiPartiteNetwork_vocabulary:Drug-Variant-Location>",
      "<http://bio2rdf.org/MultiPartiteNetwork_vocabulary:Disease-Associated-Target>",
      "<http://bio2rdf.org/MultiPartiteNetwork_vocabulary:Disease-Pathway>",
      "<http://bio2rdf.org/MultiPartiteNetwork_vocabulary:Target-Pathway>",
      "<http://bio2rdf.org/MultiPartiteNetwork_vocabulary:Drug-Target>",
      "<http://bio2rdf.org/MultiPartiteNetwork_vocabulary:Drug-Haplotype>",
      "<http://bio2rdf.org/MultiPartiteNetwork_vocabulary:Disease-Variant-Location>",
      "<http://bio2rdf.org/MultiPartiteNetwork_vocabulary:Drug-Pathway>",
      "<http://bio2rdf.org/MultiPartiteNetwork_vocabulary:Disease-Haplotype>",
      "<http://bio2rdf.org/MultiPartiteNetwork_vocabulary:Disease-Possible-Drug>"
    ),
    "P1" = c(
      "Drug",
      "Disease",
      "Disease",
      "Target",
      "Drug",
      "Drug",
      "Disease",
      "Drug",
      "Disease",
      "Disease"
    ),
    "P2" = c(
      "Variant-Location",
      "Target",
      "Pathway",
      "Pathway",
      "Target",
      "Haplotype",
      "Variant-Location",
      "Pathway",
      "Haplotype",
      "Drug"
    )
  )
  
  data <- readr::read_delim(f, delim=' ', col_names=c('V1', 'relationship', 'V2', ''))[,1:3]
  data %<>% unique()
  data %<>% dplyr::filter(relationship %in% relationships$relationship)
  data$V1 <- stringr::str_sub(data$V1, 2, -2) 
  data$V2 <- stringr::str_sub(data$V2, 2, -2)

  data$P1 <- NA
  data$P2 <- NA
  for (i in 1:nrow(relationships)) {
    data[data$relationship == relationships[i, "relationship"], "P1"] <- relationships[i, "P1"]
    data[data$relationship == relationships[i, "relationship"], "P2"] <- relationships[i, "P2"]
  }
  
  return(data)
}