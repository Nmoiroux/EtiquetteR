#source("Functions_lab_gen.R") # load tidyverse and tools packages, as well as several functions used to print the labels

# Example
require(readODS) # I store my data in an Open Document Spreadsheet (libreoffice)

path <- "W_folder/liste_ind_coll_ex.ods" # define path (or file if it is in the same folder/env.)
individuals <- readODS::read_ods(path, sheet = "Table_data") # Table of data to print (list of individuals for ex.) is stored in sheet "Table_data"
print_informations <- readODS::read_ods(path, sheet = "Print_parameters_ex1") # printing parameters (to be defined by the user)
file_out <- "W_folder/labels_mozzie.tex" # Name of the latex file to write (the same name will be used for the pdf file)

create_pdf(file_out = file_out,  ind_list = individuals , print_info = print_informations, lab_size = 15, n_col = 8, col_N_name = "N", hl_col = "orange") # Write a Latex file and create corresponding pdf

