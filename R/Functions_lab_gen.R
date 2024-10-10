#' @title Convert Sex Data to LaTeX Format
#'
#' @description
#' This function converts a string representing sex data into the appropriate LaTeX code for displaying male or female symbols in small font.
#'
#' @param sex_data A character string representing the sex of an individual. It can start with "f" or "F" for female, "m" or "M" for male, or it can be \code{NA}.
#'
#' @return A character string containing the corresponding LaTeX code: \code{"\\smallfemale"} for female, \code{"\\smallmale"} for male, or \code{NA} if the input is not recognized or is \code{NA}.
#'
#' @details
#' The function checks the first letter of the input string. If it starts with "f" or "F", it returns the LaTeX code for the female symbol. If it starts with "m" or "M", it returns the LaTeX code for the male symbol. If the input is \code{NA} or unrecognized, the function returns \code{NA}.
#'
#' @examples
#' # Example usage:
#' sex_to_latex("female") # Returns "\\smallfemale"
#' sex_to_latex("male")   # Returns "\\smallmale"
#' sex_to_latex(NA)       # Returns NA
#' 
#' @importFrom stringr str_starts
#'
#' 
sex_to_latex <- function(sex_data){
  if (is.na(sex_data)){
    sex_return <- NA
  }
  else if (str_starts(sex_data, "f|F")) {
    sex_return <- "\\smallfemale"
    }
  else if (str_starts(sex_data, "m|M")) {
    sex_return <- "\\smallmale"
  }
  else {
    sex_return <- NA
  }
  return(sex_return)
}

v_sex_to_latex <- Vectorize(sex_to_latex)

#' @title Create LaTeX Header for Mosquito Labels Document
#'
#' @description 
#' This function generates a LaTeX document header for mosquito labels and writes it to the specified output file. 
#' The header includes various LaTeX packages, document settings, metadata, and the front cover of the document. 
#' The LaTeX code is adapted from work by Samuel Brown (see https://github.com/sdjbrown/publicFiles/blob/master/labels.tex 
#' and http://the-praise-of-insects.blogspot.com/2010/03/latex-insect-labels.html).
#'
#' @param file_out A character string specifying the name of the output file to write the LaTeX header to.
#' @param lab_width An integer specifying the width (in mm) for the labels (default is 15 mm).
#' @param lab_height An integer specifying the height (in mm) for the labels (default is 9).
#' @param font_size A real (one digit) specifying the size of the font for the label (default is 4)

#' @param n_col An integer specifying the number of columns in the LaTeX document (default is 8 columns).
#'
#' @details
#' This function is primarily used for creating labels in LaTeX for mosquito specimen identification. It sets the document's layout and font sizes,
#' allowing customization of label size and number of columns on the page.
#' 
#' @examples
#' \dontrun{
#' print_header("output.tex", lab_width = 20,lab_height = 10, font_size = 5, n_col = 7)
#' }
#'
#' 
print_header <- function(file_out, lab_width = 15,lab_height = 9, font_size = 4, n_col = 8){
	# create an empty Latex output file
	file.create(file_out)
	
	# write header of the latex file
	cat(paste0("
\\documentclass[a4paper]{report}

%-------------------------------------------------------------------------------

%Set page margins

\\topmargin-20mm
\\headheight0mm
\\headsep0mm
\\textheight285mm
\\footskip0mm
\\textwidth170mm
\\oddsidemargin-11mm

%-------------------------------------------------------------------------------

%Load packages

\\usepackage{multicol}
\\usepackage{graphicx}
\\usepackage{xcolor}
\\usepackage{marvosym}
\\usepackage{environ}
\\usepackage[english]{babel}

%-------------------------------------------------------------------------------
% Set fonts and column types

\\def\\supertiny{\\font\\supertinyfont = cmss10 at 4pt \\relax \\supertinyfont} 
\\def\\supertinyitalic{\\font\\supertinyfont = cmssi10 at 4pt \\relax \\supertinyfont} 
\\def\\supertinybold{\\font\\supertinyfont = cmssbx10 at 4pt \\relax \\supertinyfont} 
\\newcommand{\\smallbold}[1]{\\supertinybold{#1}}
\\newcommand{\\locnm}[1]{\\supertinybold{#1}}
\\newcommand{\\scinm}[1]{\\supertinyitalic{#1}}

\\newcommand{\\smallprime}{\\scalebox{0.4}{$^\\prime$}}
\\newcommand{\\smalldegree}{\\scalebox{0.5}{\\textdegree}}
\\newcommand{\\smallfemale}{\\scalebox{0.5}{\\Female}}
\\newcommand{\\smallmale}{\\scalebox{0.5}{\\Male}}
\\newcommand{\\detline}{\\\\ \\smallbold{det. SDJ Brown \\the\\year}}


%Define counters and boxes

\\newcounter{speclabel@}
\\newcounter{speclabel@@}
\\newsavebox{\\TMPspeclabel}


%Set lengths and other options

\\setlength{\\parindent}{0pt} % retrait
\\setlength{\\parskip}{0ex} % espacement vertical entre deux paragraphe (effet d'un //)
\\linespread{0.3} % espacement entre les lignes d'un même paragrap
\\setlength{\\fboxsep}{1pt} % espacement entre la box de l'étiquette et la bordure

\\pagestyle{empty}

%-------------------------------------------------------------------------------
%        New environments

%speclabel creates an environment that repeats [n] times whatever is inside it.
\\newenvironment{speclabel}[1][1]{
\\setcounter{speclabel@@}{#1}
\\begin{lrbox}{\\TMPspeclabel}
        \\begin{minipage}{\\columnwidth}
                \\vspace{0.5ex}
                \\raggedright}
                {
        \\end{minipage}
\\end{lrbox}
        \\setcounter{speclabel@}{0}
        \\loop\\ifnum \\value{speclabel@} < \\value{speclabel@@}
        \\stepcounter{speclabel@}
                \\usebox{\\TMPspeclabel}
\\endgraf\\repeat}


% box_lab creates an environment for each label that is a parbox with width and height as parameters
\\NewEnviron{boxlab}{%
  \\fbox{\\parbox[t][",lab_height,"mm][t]{",lab_width,"mm}{%
    \\raggedright
      \\BODY
    %
   }}}

%-------------------------------------------------------------------------------

\\begin{document}

\\begin{multicols*}{",n_col,"} % nombre de colonnes par page

\\supertiny
"), 
			file = file_out)}

#' Print LaTeX Labels for Each Line of a Data Table
#'
#' This function prints LaTeX labels for each line of a data table based on specified information 
#' and user-defined printing parameters. It formats each label according to the provided 
#' field names, data, and formatting options. The function generates LaTeX code for printing 
#' the labels into an external file. The latex code is based on the work 
#' by Samuel Brown (see https://github.com/sdjbrown/publicFiles/blob/master/labels.tex and 
#' http://the-praise-of-insects.blogspot.com/2010/03/latex-insect-labels.html )
#'
#' @param file_out A character string specifying the path to the output file where LaTeX code will be appended.
#' @param ind_list A data frame containing the data for individuals. Each row corresponds to a specific individual.
#' @param print_info A data frame specifying the printing parameters, including which fields to print, formatting options, and field names.
#' @param line_n An integer indicating the row number in \code{ind_list} for which the labels should be created.
#' @param col_N_name A character string specifying the name of the column in  \code{ind_list} that indicates the number of individuals per row
#' @param hl_col A character string specifying the color to be used for text highlighting
#'
#' @details The function retrieves data from a specified row in \code{ind_list} and matches it with the corresponding print parameters
#' in \code{print_info}. It formats each label using the LaTeX code according to the user-defined options in \code{print_info}, 
#' such as whether to italicize or bracket certain fields, and whether to include field names before the information. 
#' The function generates LaTeX code for individual labels and appends it to the specified output file.
#'
#' @return The function appends LaTeX code to the file specified in \code{file_out}.
#' It does not return anything in R.
#'
#' @examples
#' # Example usage:
#' # print_line("output.tex", ind_list, print_info, line_n = 1)
#' @importFrom dplyr arrange mutate if_else filter select group_by summarise
#' @importFrom magrittr %>%
#' @importFrom purrr pmap_chr
#' @importFrom stringr str_c
#' 
#### create labels per each line of the table

print_line <- function(file_out, ind_list, print_info, line_n, col_N_name = "N", hl_col = "orange"){

	v_ind <- ind_list[line_n,] %>% as.vector() %>% unlist() # retrieve data on the specified row number
	
	print_info <- print_info %>% dplyr::arrange(factor(field_name, levels = names(v_ind)))
	
	ifelse(identical(names(v_ind), print_info$field_name), # verify that var. names correspond between data table and print parameters table
				 print_info$data <- unname(v_ind),
				 print("Field names do not correspond or are not in the same order")
	)
	
	N_labels <- max(print_info$label_no, na.rm = TRUE) # number of labels to print (per individuals)
	N_val <- v_ind[col_N_name] %>% as.integer()
	N_individuals <- ifelse(is.na(N_val), 0, N_val) # retrieve the number of individuals that fit information in this row (to be used to duplicate labels)
	
	
	print_info <- print_info %>% # latex code for each information is prepared according to user-defined print parameters
		dplyr::mutate(print_txt = dplyr::if_else(print == 1, data, NA, NA)) %>% # retrieve data only for column containing information to print
	  dplyr::mutate(print_txt = dplyr::if_else(print_sex_symbol == 1 , v_sex_to_latex(print_txt), print_txt, print_txt)) %>% # add Latex codes for sex symbols
		dplyr::filter(!(is.na(print_txt))) %>% # filter dataset to keep only information to be printed (remove blank fields)
		dplyr::mutate(print_txt = dplyr::if_else(print_opt_it == 1, stringr::str_c("{\\scinm ", print_txt, "}"), print_txt, print_txt)) %>% # add latex code for italic
		dplyr::mutate(print_txt = dplyr::if_else(print_opt_par == 1, stringr::str_c("(", print_txt, ")"), print_txt, print_txt)) %>% # add brackets
		dplyr::mutate(print_txt = dplyr::if_else(print_opt_hl == 1, stringr::str_c("\\begingroup\\fboxsep=0pt\\colorbox{",hl_col,"}{",print_txt,"}\\endgroup"), print_txt, print_txt)) %>% 
		dplyr::mutate(field_name_to_print = dplyr::if_else(print_field_name == 1 & is.na(field_name_to_print), field_name, field_name_to_print)) %>% # define field name to print before information, if specified
		dplyr::mutate(print_txt = purrr::pmap_chr(list(print_txt, print_field_name,field_name_to_print), function(x,y,z) dplyr::if_else(y == 1, 
																																																							 stringr::str_c(z," ", x), x, x))) %>%# add a field name before information to print, if specified
		dplyr::mutate(print_txt = dplyr::if_else(line_break == 1, stringr::str_c(print_txt, "\\","\\"), print_txt, print_txt)) # add latex code for break line
	
	# concatenate Latex code / Info to print for each individual label
	labels <- print_info %>% 
		dplyr::filter(print==1) %>%
		dplyr::arrange(label_no, order_lab) %>% 
		dplyr::select(label_no, print_txt) %>%
		dplyr::group_by(label_no) %>%
		dplyr::summarise(print_lab = paste(print_txt, collapse = " ")) 
	
	# add code to the Latex file
	cat(paste0("
	\\begin{speclabel}[",N_individuals,"]"
	), 
	file = file_out, append = TRUE)
	
	for (nlab in 1:N_labels){
		cat(paste0("
			 \\begin{boxlab}
		           ",
							 labels[nlab,2],"
			 \\end{boxlab}
							 "
		), 
		file = file_out, append = TRUE)
	}
	
	cat(paste0("
			 \\end{speclabel}"
	), 
	file = file_out, append = TRUE)
}

#' Append the Closing Section to a LaTeX Document
#'
#' This function writes the closing commands to a LaTeX document. Specifically, 
#' it appends the end of a `multicols*` environment and the `document` environment.
#'
#' @param file_out A character string specifying the path to the output file where 
#' the LaTeX closing commands will be appended.
#'
#' @details The function appends LaTeX commands for ending a multiple-column layout 
#' (using the `multicols*` environment) and the document. It ensures the proper 
#' closure of a LaTeX document that was generated by the preceding steps.
#'
#' @return This function appends LaTeX code to the file specified in \code{file_out}. 
#' It does not return any output in R.
#'
#' @examples
#' # Example usage:
#' # print_bottom("output.tex")
#'
#' @export
print_bottom <- function(file_out){
	# write bottom of the latex file
	cat(paste0("
	\\end{multicols*}
	\\end{document}
"), 
			file = file_out, append = TRUE)}

#' @title Generate a Complete LaTeX Document and Compile it into a PDF
#'
#' @description
#' This function generates a LaTeX document by sequentially adding a header, printing 
#' labels for each line of data, appending a footer, and then compiling the LaTeX code 
#' into a PDF document.
#'
#' @param file_out A character string specifying the path to the LaTeX file that will be generated 
#' and compiled into a PDF.
#' @param ind_list A data frame containing individual data. Each row represents data 
#' for one label to be printed in the LaTeX document.
#' @param print_info A data frame specifying the parameters for printing, including field names, 
#' formatting options, and what data to print.
#' @param lab_width An integer specifying the width (in mm) for the labels (default is 15 mm).
#' @param lab_height An integer specifying the height (in mm) for the labels (default is 9).
#' @param font_size A real (one digit) specifying the size of the font for the label (default is 4)
#' @param n_col An integer specifying the number of label columns in the LaTeX document (default is 8 columns).
#' @param col_N_name A character string specifying the column name to be used for labels duplication (default is "N").
#' @param hl_col A character string specifying the highlight color for specific elements (default is "orange").
#'
#' @details 
#' This function first calls \code{print_header} to write the beginning of the LaTeX 
#' document. Then, it iterates over each row of \code{ind_list}, calling \code{print_line} 
#' to generate labels according to the provided \code{print_info}. After all labels are printed, 
#' it appends the LaTeX document footer using \code{print_bottom}. Finally, it compiles the 
#' LaTeX document into a PDF using \code{pdflatex}.
#'
#' @return 
#' The function creates a LaTeX file, compiles it into a PDF, and saves the output 
#' to the specified location. It does not return any value in R.
#'
#' @examples
#' # Example usage:
#' # create_pdf("output.tex", ind_list, print_info, lab_width = 15, lab_height = 9, font_size = 4, n_col = 8, col_N_name = "N", hl_col = "orange")
#'
#' @export

create_pdf <- function(file_out, ind_list, print_info,lab_width = 15, lab_height = 9, font_size = 4, n_col = 8, col_N_name = "N", hl_col = "orange"){
	# Step 1: Write the LaTeX document
	print_header(file_out = file_out, lab_width, lab_height, font_size, n_col)
	
	# Step 2: Generate labels for each individual in the list
	for (num in 1:nrow(ind_list)) {
		print_line(file_out = file_out, ind_list = ind_list, print_info = print_info, line_n = num, col_N_name = "N", hl_col = "orange")
	}
	
	# Step 3: Append the footer to the LaTeX document
	print_bottom(file_out = file_out)
	
	# Step 4: Compile the LaTeX document into a PDF using pdflatex
	compile_result <- system(sprintf("pdflatex -interaction=nonstopmode %s", file_out), intern = TRUE)
	
	# Step 5: Check if the PDF generation was successful
	if (any(grepl("Output written on", compile_result))) {
		message("PDF successfully generated.")
	} else {
		message("LaTeX compilation failed. Please ensure that pdflatex is installed and accessible in your PATH.")
		# Print the error output from pdflatex
		print(compile_result)
	}
}
