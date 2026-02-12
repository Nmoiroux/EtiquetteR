#' Example mosquito collection dataset for label generation
#'
#' A toy dataset representing mosquito specimens collected in southern France
#' and intended for pinning and label printing with \code{create_pdf()}.
#'
#' The dataset follows the standard input structure expected by
#' \pkg{EtiquetteR}, with one row per specimen (or per group of identical
#' specimens when \code{N > 1}). It includes collection metadata,
#' taxonomic identification, sex, geographic coordinates (in DMS format),
#' and a unique specimen identifier.
#'
#' This dataset can be used to test label layout, formatting, and PDF
#' generation without requiring external files.
#'
#' @format A data frame with 24 rows and 12 variables:
#' \describe{
#'   \item{rec_date}{Collection date (character). Example: \code{"06/2024"}.}
#'   \item{rec_place}{Locality name, optionally including administrative unit.}
#'   \item{rec_cntry}{Country of collection.}
#'   \item{Y}{Latitude in degrees-minutes-seconds (DMS) format (character).}
#'   \item{X}{Longitude in degrees-minutes-seconds (DMS) format (character).}
#'   \item{rec_name}{Collector name.}
#'   \item{id_name}{Identifier (person who identified the specimen).}
#'   \item{sex}{Biological sex of the specimen (e.g., \code{"Male"}, \code{"Female"}).}
#'   \item{genus}{Genus name (e.g., \emph{Culiseta}, \emph{Culex}).}
#'   \item{subgenus}{Subgenus name (may be empty).}
#'   \item{species}{Species epithet.}
#'   \item{N}{Number of specimens sharing identical metadata.}
#'   \item{id}{Unique specimen identifier (integer).}
#' }
#'
#' @details
#' The dataset contains specimens of \emph{Culiseta (Cal.) longiareolata}
#' and \emph{Culex pipiens}.
#'
#' @source Simulated dataset for demonstration purposes.
#'
#' @examples
#' data(mosquito_collection)
#' head(mosquito_collection)
#'
#' # Example: generate labels directly from the dataset
#' # create_pdf(
#' #   data = mosquito_example,
#' #   file_output = "mosquito_labels.pdf"
#' # )
#'
#' @name mosquito_collection
#' @docType data
"mosquito_collection"


#' Example print parameter table for label formatting
#'
#' A toy parameter table defining how specimen data fields are arranged,
#' formatted, and distributed across labels when using \code{create_pdf()}.
#'
#' This dataset illustrates the structure expected by \pkg{EtiquetteR} to
#' control label composition (field selection, ordering, formatting options,
#' line breaks, highlighting, and symbol printing).
#'
#' Each row corresponds to a field present in the specimen data table and
#' specifies whether and how it should be printed.
#'
#' @format A data frame with 13 rows and 10 variables:
#' \describe{
#'   \item{field_name}{Name of the field in the `mosquito_collection data` table.}
#'   \item{print}{Logical (0/1). Whether the field is printed on labels.}
#'   \item{label_no}{Numeric. Label number on which the field is printed
#'     (e.g., 1 = identification label, 2 = locality/date label,
#'     3 = collector/identifier label).}
#'   \item{order_lab}{Numeric. Order of appearance within the label.}
#'   \item{prefix}{Character string added before the field value
#'     (e.g., \code{"leg."}, \code{"det."}, \code{"no."}).}
#'   \item{print_opt_it}{Logical (0/1). Whether the field should be printed
#'     in italics (typically for taxonomic names).}
#'   \item{print_opt_par}{Logical (0/1). Whether the field should be printed
#'     in parentheses (commonly used for subgenus).}
#'   \item{line_break}{Logical (0/1). Whether a line break follows the field.}
#'   \item{print_opt_hl}{Logical (0/1). Whether the field should be highlighted.}
#'   \item{print_sex_symbol}{Logical (0/1). Whether to print a sex symbol
#'     instead of the full sex text.}
#' }
#'
#' @details
#' This example configuration produces three labels per specimen:
#' \enumerate{
#'   \item Taxonomic label (genus, subgenus, species, sex).
#'   \item Collection label (date and locality).
#'   \item Collector/identifier label (collector, determiner, specimen number).
#' }
#'
#' Taxonomic names are printed in italics, the subgenus is enclosed in
#' parentheses, and sex can optionally be rendered as a biological symbol.
#'
#' @source Simulated parameter table for demonstration purposes.
#'
#' @examples
#' data(print_parameters)
#' print(print_parameters)
#'
#' # Example usage with create_pdf():
#' # create_pdf(
#' #   data = mosquito_collection,
#' #   print_parameters = print_parameters,
#' #   file_output = "mosquito_labels.pdf"
#' # )
#'
#' @name print_parameters
#' @docType data
print_parameters


