#source("Functions_lab_gen.R") # load tidyverse and tools packages, as well as several functions used to print the labels

# Example
require(readODS) # I store my data in an Open Document Spreadsheet (libreoffice)

path <- "W_folder/liste_ind_coll_ex.ods" # define path (or file if it is in the same folder/env.)
individuals <- readODS::read_ods(path, sheet = "Table_data") # Table of data to print (list of individuals for ex.) is stored in sheet "Table_data"
print_informations <- readODS::read_ods(path, sheet = "Print_parameters_ex1") # printing parameters (to be defined by the user)
file_out <- "W_folder/labels_mozzie.tex" # Name of the latex file to write (the same name will be used for the pdf file)

create_pdf(file_out = file_out,  ind_list = individuals , print_info = print_informations, lab_size = 15, n_col = 8, col_N_name = "N", hl_col = "orange") # Write a Latex file and create corresponding pdf

###
library(tidyverse)
library(stringr)
v_ind <- individuals [1,] |> as.vector() |> unlist() 
v_ind %>%
  gsub(pattern = "°", replacement = "\\\\smalldegree",.) %>%
  mutate(sex = gsub(pattern = "Female", replacement = "\\\\smallfemale",sexe))

if("N" %in% names(individuals)){
  individuals$N <- individuals$N %>% as.integer
} else {
  individuals$N <- individuals$N %>% as.integer(1)
}

ind_list <- individuals
print_info <- print_informations
col_N_name <- "N"
v_ind <- ind_list[5,] %>% as.vector() %>% unlist() # r
ifelse(identical(names(v_ind), print_info$field_name), # verify that var. names correspond between data table and print parameters table
       print_info <- print_info %>% mutate(data = as.character(v_ind)),
       print("Field names do not correspond or are not in the same order")
)

N_val <- v_ind[col_N_name] %>% as.integer()
N_individuals <- ifelse(is.na(N_val), 0, N_val) # retrieve the number of individuals that fit information in this row (to be used to duplicate labels)

print_info %>% # latex code for each information is prepared according to user-defined print parameters
  dplyr::mutate(print_txt = dplyr::if_else(print == 1, data, NA, NA)) %>% # retrieve data only for column containing information to print
  dplyr::mutate(print_txt = dplyr::if_else(print_sex_symbol == 1 , v_sex_to_latex(print_txt), print_txt, print_txt)) %>% # add Latex codes for sex symbols
  dplyr::filter(!(is.na(print_txt))) %>% # filter dataset to keep only information to be printed (remove blank fields)
  dplyr::mutate(print_txt = dplyr::if_else(print_opt_it == 1, stringr::str_c("{\\scinm ", print_txt, "}"), print_txt, print_txt)) %>% # add latex code for italic
  dplyr::mutate(print_txt = dplyr::if_else(print_opt_par == 1, stringr::str_c("(", print_txt, ")"), print_txt, print_txt)) 