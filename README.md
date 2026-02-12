[![DOI](https://zenodo.org/badge/887177944.svg)](https://doi.org/10.5281/zenodo.14987479)

## EtiquetteR: Generate Labels for Insect Pinning

### Overview
EtiquetteR is an R package designed to streamline the creation of high-quality labels for insect pinning. By taking a dataset as input, EtiquetteR generates printable labels in PDF format, helping researchers and entomologists maintain accurate and standardized specimen records.

For enhanced accessibility, the package includes a user-friendly Shiny application, **InsectLabelR**, which provides a graphical interface for generating labels without requiring programming expertise.

### Features
- Create insect pinning labels from dataset inputs.
- Output labels as PDF files, ready for printing.
- Flexible label formatting options.
- User-friendly GUI via the Shiny app InsectLabelR.

---

### Installation
To install the EtiquetteR package, use the following command in R:
```R
# Install from GitHub
devtools::install_github("Nmoiroux/EtiquetteR")
```

---

### Usage
#### Using the Shiny App: **InsectLabelR**
The Shiny app provides an intuitive interface for generating insect pinning labels. You can access it online at [InsectLabelR](https://nicolas-moiroux.shinyapps.io/InsectLabelR/) or run it locally:
```R
shiny::runApp(system.file("shiny", package = "EtiquetteR"))
```

**Steps to Use the Shiny App**:
1. **Upload Data**: Upload a `.csv`, `.xlsx`, or `.ods` file containing your insect data.
   - If using `.xlsx` or `.ods`, select the appropriate sheet containing the dataset.
   - Example datasets are available in the [EtiquetteR repository](https://github.com/Nmoiroux/EtiquetteR/tree/main/inst/extdata).
2. **Configure Printing Parameters**: Use a pre-configured table or manually fill out a printing parameters table, specifying:
   - Which fields to print
   - Number of labels per individual
   - Printing order
   - Text formatting (e.g., italics for genus/species).
3. **Customize Labels**: Adjust label width, height, font size, and more.
4. **Generate PDF**: Download the formatted labels as a PDF file.

---

### Input Format
Your dataset must include:
1. **Data Table**: A table with one row per insect and columns describing attributes (e.g., species, collection date).
   - Supported formats: `.csv` (tab-delimited), `.xlsx`, `.ods`.
   - Column names are flexible.
2. **Printing Parameters Table**: Specifies how data fields appear on labels.
   - Formats: as a sheet in the `.xlsx` or `.ods` file containing the Data table, or as a `.csv` file (tab-delimited).
   - Includes the following columns: `field_name`, `print`, `label_no`, `order_lab`, and more.
   - Can be configured offline or within the app. 

Example data and parameter tables are provided in the [EtiquetteR repository](https://github.com/Nmoiroux/EtiquetteR/tree/main/inst/extdata).

---

### Example Workflow
1. Download the example dataset: [liste_ind_coll_ex.ods](https://github.com/Nmoiroux/EtiquetteR/blob/main/inst/extdata/liste_ind_coll_ex.ods).
2. Upload the file and select the sheet "Table_data".
3. Set printing parameters by selecting "Print_parameters_ex1" sheet or configure them manually.
4. Adjust label size, font, and layout as needed.
5. Generate and download the PDF labels.

---

### Usage in R

EtiquetteR provides programmatic access to its core functionality. Below is an example using the `create_pdf()` function with a dataset embedded in the package.

#### Example: Generating Labels with Embedded Dataset

```R
# Example dataset path (included in the package)
data_path <- system.file("extdata", "liste_ind_coll_ex.ods", package = "EtiquetteR")

# Load data table
data_table <- readODS::read_ods(data_path, sheet = "Table_data")

# Load print parameter data
par_table <- readODS::read_ods(data_path, sheet = "Print_parameters_ex1")

# Define output file path for the PDF
output_pdf <- "example_labels.pdf"

# Generate labels
EtiquetteR::create_pdf(
  file_out = output_pdf,            # Name of pdf file
  ind_list = data_table ,           # Table of data
  print_info = par_table,           # Table of printing parameters
  font_size = 5,                    # Font size
  lab_width = 15,                   # Width of the labels in mm
  lab_height =12,                   # Height of the labels in mm
  n_col = 8,                        # Number of label columns per row
  hl_col = "orange"                 # Color for highlighted text
) 
```

This script:
1. Loads an example `.ods` dataset included in the package.
2. Configures the data and parameter sheets.
3. Defines label size, font, and layout.
4. Outputs the generated labels as a PDF file.

#### Expected Output
The generated `example_labels.pdf` file will contain insect pinning labels formatted according to the specified parameters.



---

### Dependencies
- R version >= 4.0.0
- Required packages: `dplyr`, `purrr`, `stringr`, `magrittr`

### Troubleshooting
- Ensure your dataset and parameters follow the specified structure.
- Avoid special characters in field names to prevent LaTeX rendering issues.

---
