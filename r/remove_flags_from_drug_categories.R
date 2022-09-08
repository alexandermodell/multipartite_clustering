require(readr)
require(stringr)

remove_flags_from_drug_categories <- function(input_filepath, output_filepath) {
  drug_categories <- readr::read_csv(input_filepath, show_col_types = FALSE)
  drug_categories_no_flags <- drug_categories
  drug_categories_no_flags$category <- stringr::str_extract(drug_categories_no_flags$category, "[^,]*")
  drug_categories_no_flags <- unique(drug_categories_no_flags)
  readr::write_csv(drug_categories_no_flags, output_filepath)
}
