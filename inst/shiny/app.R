# Install/load necessary libraries
library(shiny)
library(markdown) # display user guide
library(readr) # reading csv file
library(readxl) # reading xlsx files
library(readODS) # For handling .ods files
library(EtiquetteR) # Generate latex then pdf file
library(DT) # display table
library(editbl) # diplay editable table
library(shinyjs) # ?? 

# hack the editbl package to render table with trash buttons masked
mybuttonsHTML <- "<div class=\"btn-group\">\n   <button id=\"%2$sedit_row_%1$s\" type=\"button\" class=\"btn btn-default action-button\" style=\"background-color: white\" onclick=\"get_id(this.id, &#39;%2$s&#39;);&#10;                          Shiny.setInputValue(&quot;%2$sedit&quot;, Math.random(), {priority: &quot;event&quot;});\">\n    <i class=\"far fa-pen-to-square\" role=\"presentation\" aria-label=\"pen-to-square icon\"></i>\n    \n  </button>\n</div>"
# above is the - modified - output of the hidden function 'createButtonsHTML'
# below is the modified fun 'createButtons' in order to render the modified html code
mycreateButtons <- function(suffix, ns){
  sprintf(
    # Can be generated with createButtonsHTML
    mybuttonsHTML,
    suffix,
    ns("")
  )
}
#below replace the function 'createButtons' in the editbl package by above 'mycreateButtons'
assignInNamespace("createButtons", mycreateButtons  , ns="editbl", pos="package:editbl")

empty_par <- function(var){
  liste_ind_coll_ex <- system.file("extdata", "liste_ind_coll_ex.ods", package = "EtiquetteR")
  pp_EtiquetteR <- read_ods(liste_ind_coll_ex, sheet = "Print_parameters_ex2")
  n_col <- ncol(pp_EtiquetteR)
  n_row <- length(var)
  pp_empty <- as.data.frame(pp_EtiquetteR[0,])
  a <- matrix(data = c(var,rep(NA,n_row*(n_col-1))),ncol = n_col)
  pp_empty[1:length(var),] <- NA
  pp_empty$field_name <- var
  return(pp_empty)
}


# Define UI for the app
ui <- fluidPage(
  titlePanel("Generate labels for your pinned insects"),
  
  # Sidebar layout with input and output elements
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", label = #tooltip(trigger = list("Upload dataset", bs_icon("info-circle", title = "About")),
                  "Select the dataset (.ods, .xlsx or tab-sep .csv) that contains your data", # File selector for dataset
                accept = c(".csv", ".ods", ".xlsx")),
      #uiOutput("q_upload_par_selector"), # Dynamic UI for selecting the source of printing paramters when datatabl is a CSV file (manually defined or file to be provided)
      uiOutput("sheet_selector"),  # Dynamic UI for selecting data table sheet
      uiOutput("sheet_selector_par"),  # Dynamic UI for selecting printing parameters sheet
      uiOutput("file2_selector"), # Dynamic UI for printing parameter file selection
      numericInput("lab_width", "Label width (mm)", value = 15, min = 1, width = '50%', step = 1),
      numericInput("lab_height", "Label height (mm)", value = 8, min = 1, width = '50%', step = 1),
      numericInput("font_size", "Font size", value = 5, min = 1, width = '50%', step = 0.5),
      numericInput("N_col", "Number of columns per page", value = 8, min = 1, width = '50%', step = 1),
      textInput("hl_col", "Highlighting color", value = "orange"), # color used for highlighting
      uiOutput("field_N_selector"),  # Dynamic UI for selecting column indicating no of replication
      downloadButton("download_pdf", "Download PDF"), # download button
      width = 3),
    
    ## Main Panel ----
    mainPanel( 
      tabsetPanel( # mutiple panel
        tabPanel( ## tab user guide----
                  "User guide", 
                  value = "panel3",
                  includeMarkdown("user_guide.Rmd")
                  ), 
        tabPanel(## tab user guide FR----
                 "Guide de l'utilisateur", 
                 value = "panel5",
                 includeMarkdown("user_guide_fr.Rmd")
                 ),
        tabPanel("Data table", dataTableOutput('datatable'), value = "panel1"), # Tab data table----
        tabPanel( # Tab printing parameters tables----
                  "Printing parameters table (editable)", dataTableOutput('print_info'), value = "panel2",
                  eDTOutput('print_info', width="100%", height = "100%")
        ), 
        tabPanel("About", value = "panel4", # Tab About----
                 h3("About"),
                 tags$p("This shiny app is a friendly GUI for the EtiquetteR package : Moiroux (1,2), Nicolas, and Nil Rahola (1,3). ‘EtiquetteR: An R Package for Designing Labels for Insect in Collections’, 2024. https://github.com/Nmoiroux/EtiquetteR .
"),
                 tags$p("The EtiquetteR package allow to generate a LaTeX document from a data table and a printing parameters table, and then compiling the LaTeX code 
                        into a PDF document. The LaTeX code is adapted from work by Samuel Brown (see https://github.com/sdjbrown/publicFiles/blob/master/labels.tex 
                                                                                                  and http://the-praise-of-insects.blogspot.com/2010/03/latex-insect-labels.html)."),
                 tags$p("(1) MIVEGEC, Univ. Montpellier, IRD, CNRS, Montpellier, France ; (2) Pôle de zoologie médicale, Institut Pasteur de Dakar, Sénégal ; (3) Institut Pasteur de Madagascar, Antananarivo, Madagascar"),
                 
                 h4("Aknowledgements"),
                 tags$p("ANR project DIV-YOO (Diversity of mosquitoes in West Africa)")), 
        id = "panel"
      ),
      width = 9
    )
  )
)

# Define server logic----
server <- function(input, output, session) {
  
  # Reactive value that stores printing parameters tables (either uploaded or edited)
  print_par = reactiveValues(df = NULL)
  
  # Reactive function to get sheet names and render sheet selector
  observeEvent(input$file1,{
    #req(input$file1)
    file_ext <- tools::file_ext(input$file1$datapath)
    # Get sheet names depending on the file type
    sheets <- NULL
    if (file_ext == "xlsx") {
      sheets <- excel_sheets(input$file1$datapath)
      # case 1
    } else if (file_ext == "ods") {
      sheets <- list_ods_sheets(input$file1$datapath)  # Custom function to get sheet names
      # case  2
    } else if (file_ext == "csv") {
      output$sheet_selector_par <- renderUI({
        selectInput("sheet_par", "Select printing parameters table source", choices = c("choose...", "manually fill out", "csv file"))
      })
      disable("sheet_selector")
    }
    
    # Render sheet selector if there are multiple sheets
    if (!is.null(sheets)) {
      output$sheet_selector <- renderUI({
        selectInput("sheet", "Select data sheet", choices = c("choose...",sheets))
      })
    }
    # Render sheet selector if there are multiple sheets
    if (!is.null(sheets)) {
      output$sheet_selector_par <- renderUI({
        selectInput("sheet_par", "Select printing parameters source", choices = c("choose...","manually fill out","csv file", sheets))
      })
    }
  })
  
  # should fileInput for print parameter file be enable ? (according to selectInput q_upload_par)
  observeEvent(input$sheet_par,{
    req(input$sheet_par)
    if(input$sheet_par == "csv file"){
      output$file2_selector <- renderUI({
        fileInput("file2", "Upload your print parameters table (CSV)",
                  accept = c(".csv"))
      })
      
    } else {
      disable("file2_selector")
      #case  3 # to be used for print_information source of data
    }
  })
  
  observeEvent(input$file2,{
    req(input$file2)
    file_ext_2 <- tools::file_ext(input$file2$datapath)
    #case 4
  })
  
  observeEvent(individuals(),{
    req(individuals()) # require data table exists (has been upload and sheet selected)
    
    output$datatable <- renderDT(individuals(),options = list( # display data table
      pageLength = 20)
      #, editable ="cell", selection = list(target = 'cell') # if we want the table to be editable
    )
    col_name <- colnames(individuals()) # retrieve column names for duplication number field
    output$field_N_selector <- renderUI({
      selectInput("col_N_name", "Select column for number of label replication", choices = c("do not replicate",col_name))
    })
  })
  
  # Reactive function to read user input dataset
  individuals <- reactive({
    
    req(input$file1)
    file_ext <- tools::file_ext(input$file1$datapath)
    
    if (file_ext == "csv") { # use the correct function to read the data table according to file extension (& selected sheet)
      read_tsv(input$file1$datapath) # require tab-delimited csv
    } else if (file_ext == "ods") {
      req(input$sheet)
      if (input$sheet != "choose..."){ # if a file is selected
        read_ods(input$file1$datapath, sheet = input$sheet)}  # Use selected sheet
    } else if (file_ext == "xlsx") {
      req(input$sheet)
      if (input$sheet != "choose..."){ # if a file is selected
        read_xlsx(input$file1$datapath, sheet = input$sheet)}  # Use selected sheet
    }
  })
  
  observeEvent(input$file2,{
    print(input$file2$datapath)
  })
  # # Load the print_parameter table or template from EtiquetteR package
  print_informations <- reactive({
    col_name <- colnames(individuals())
    print_params <- NULL
    req(input$file1)
    f_ext <- tools::file_ext(input$file1$datapath) # retrieve file extension of the data table source
    #print(paste(f_ext, input$file1$datapath, input$sheet_par, input$file2$datapath))
    if (f_ext == "xlsx" ){ #if data table is from an excel file
      req(input$sheet_par) # retrieve user defined sheet name for printing parameter
      if(!(input$sheet_par %in% c("choose...","manually fill out", "csv file"))) { # user can select another sheet, if a sheet is selected
        print_params <- read_xlsx(input$file1$datapath, sheet = input$sheet_par) # read the table
      } else if (input$sheet_par == "manually fill out"){ # if user want to manually fill the table
        print_params <- empty_par(col_name) # retrieve only data table column names as row 
      } else if(input$sheet_par == "csv file" & isTruthy(input$file2)){
        #req(input$file2)
        print_params <- read_tsv(input$file2$datapath)
      } else {
        print_params <- NULL # in all other case, an empty table to be manualy filled is provided
      }
      
    } else if (f_ext == "ods" ){ # if ods dataset
      req(input$sheet_par)
      if(!(input$sheet_par %in% c("choose...","manually fill out", "csv file"))){
        print_params <- read_ods(input$file1$datapath, sheet = input$sheet_par)
      } else if (input$sheet_par == "manually fill out"){
        print_params <- empty_par(col_name)
      } else if(input$sheet_par == "csv file"){
        req(input$file2)
        print_params <- read_tsv(input$file2$datapath)
      } else {
        print_params <- NULL # in all other case, an empty table to be manualy filled is provided
      }
    } else if (f_ext == "csv"){ # if data table is from a csv file
      req(input$sheet_par)
      if(input$sheet_par == "csv file"){
        req(input$file2)
        print_params <- read_tsv(input$file2$datapath)
      } else if (input$sheet_par == "manually fill out"){
        print_params <- empty_par(col_name)
      } else {
        print_params <- NULL # in all other case, an empty table to be manualy filled is provided
      }
    } 
    
    return(print_params)
  })
  
  
  # observe if printing information table has been generated and display
  observeEvent(print_informations(),{
    req(print_informations()) # verify that the reactive function has a result
    print_par$df <- print_informations()
    modifiedData <- eDT(id = 'print_info', data = print_informations(),
                        options = list(dom = "Bfrtlip", keys = TRUE, ordering = TRUE,
                                       autoFill = list(update = FALSE, focus = "focus"),
                                       buttons = list("undo", "redo", "save", 
                                                      list(
                                                        extend = "csv",
                                                        exportOptions = list(
                                                          columns = "1,2,3,4,5,6,7,8,9,10"
                                                        ),
                                                        fieldSeparator = "\t",
                                                        title = "InsectLabelR_print_parameters"
                                                      )
                                       ), pageLength = 20),
                        filter = "none")
    updateTabsetPanel( # update active panel
      session = getDefaultReactiveDomain(),
      "panel",
      selected = "panel2"
    )
  })
  
  #if Printing parameter table is edited
  observeEvent(input[["print_info-DT_cell_edit"]], {
     print_par$df <<- editData(print_par$df, input[["print_info-DT_cell_edit"]])
  })

  
  # Observe the button click and generate the PDF
  output$download_pdf <- downloadHandler(
    filename = function() { # generate file name for the pdf file
      paste("labels-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      
      df_ind <- individuals()
      print(print_par$df)
      # call EtiquetteR::create_pdf function
      create_pdf(
        file_out = "labels.tex",
        ind_list = df_ind,
        print_info = print_par$df,
        lab_height = input$lab_height,
        lab_width = input$lab_width,
        font_size = input$font_size,
        n_col = input$N_col,
        hl_col = input$hl_col,
        col_N_name = ifelse(input$col_N_name=="do not replicate",NA,input$col_N_name)
      )
      
      # Move the compiled PDF to the path requested by downloadHandler
      file.copy("labels.pdf", file, overwrite = TRUE) 
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
