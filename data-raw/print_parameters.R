## code to prepare `print_parameters` dataset

# Path
data_path <- system.file("extdata", "liste_ind_coll_ex.ods", package = "EtiquetteR")

# Load print parameter data
print_parameters <- readODS::read_ods(data_path, sheet = "Print_parameters_ex1")


usethis::use_data(print_parameters, overwrite = TRUE)
