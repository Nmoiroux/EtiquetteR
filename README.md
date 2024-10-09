# EtiquetteR
Create Labels for Insect collection

## data table requirements
A column named 'N' indicated the number of label to print per row is required (integer values), if not provided it is created and filled with "1" (i.e. one label per row will be printed). If this field contains non-integer values, they will be converted (using as.integer() function).
If you want to display sex of specimen using symbols, make sure the column is filled with either "F"/"M"/"female"/"male"/"Female"/"Male" string character values.

## printing parameter table
First column should be named 'fied_name' and filled with ALL data table column names.
