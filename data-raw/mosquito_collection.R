## code to prepare `mosquito_collection` dataset

# Path
data_path <- system.file("extdata", "liste_ind_coll_ex.ods", package = "EtiquetteR")

# Load data table
mosquito_collection <- readODS::read_ods(data_path, sheet = "Table_data")



usethis::use_data(mosquito_collection, overwrite = TRUE)
