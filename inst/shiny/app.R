# Install/load necessary libraries
library(shiny)
library(readr) # reading csv file
library(readxl) # reading xlsx files
library(readODS) # For handling .ods files
library(EtiquetteR) # Generate latex then pdf file
library(DT) # display tables
library(shinyjs) 

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
  titlePanel("Generate labels for your pinning insects"),

  # Sidebar layout with input and output elements
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", label = #tooltip(trigger = list("Upload dataset", bs_icon("info-circle", title = "About")),
        "Select the dataset (.ods, .xlsx or tab-sep .csv) that contains your data", # File selector for dataset
                accept = c(".csv", ".ods", ".xlsx")),
      uiOutput("q_upload_par_selector"), # Dynamic UI for selecting the source of printing paramters when datatabl is a CSV file (manually defined or file to be provided)
      uiOutput("file2_selector"), # Dynamic UI for printing parameter file selection
      uiOutput("sheet_selector"),  # Dynamic UI for selecting data table sheet
      uiOutput("sheet_selector_par"),  # Dynamic UI for selecting printing parameters sheet
      numericInput("lab_width", "Label width (mm)", value = 15, min = 1, width = '50%', step = 1),
      numericInput("lab_height", "Label height (mm)", value = 8, min = 1, width = '50%', step = 1),
      numericInput("font_size", "Font size", value = 5, min = 1, width = '50%', step = 0.5),
      numericInput("N_col", "Number of columns per page", value = 8, min = 1, width = '50%'),
      textInput("hl_col", "Highlighting color", value = "orange"), # color used for highlighting
      uiOutput("field_N_selector"),  # Dynamic UI for selecting column indicating no of replication
      downloadButton("download_pdf", "Download PDF"), # download button
      width = 3),
    
    ## Main Panel ----
    mainPanel( 
      tabsetPanel( # mutiple panel
        tabPanel(
          "User guide", 
          value = "panel3",
          h3("User Guide: Generate Labels for Pinning Insects Using Shiny App"),
          tags$p("This guide will help you use the app to generate insect pinning labels based on your dataset and configure the label printing parameters."),
          
          h4("1. Upload Your Dataset"),
          tags$p("Supported file formats: .csv (tab-delimited), .xlsx, .ods"),
          tags$ol(
            tags$li("Click on the 'Browse' button and upload the file containing your insect data. If its for test purpose, you can use a dataset from the ETiquetteR package (that also contains examples of printing parameter tables, see step 2 below) available here: https://github.com/Nmoiroux/EtiquetteR/blob/main/inst/extdata/liste_ind_coll_ex.ods"),
            tags$li("If your dataset has multiple sheets, select the appropriate sheet from the drop-down menu. Your data should be diplayed in the 'Data table' tab")
          ),
          
          h4("2. Configure the Printing Parameters Table"),
          tags$p("The printing parameters table defines how the information in your dataset will be displayed on the labels. You can either upload a pre-configured table (stored as a sheet in the uploaded ods or xlsx file, or as a csv file) that can be edited, or fill out the table manually (select 'manually fill out' in the drop-down menu). Your data should be diplayed in the 'Print parameters table' tab. Below is a breakdown of each column in the table, along with how to configure them:"),
          tags$p(tags$strong("Note: "), "For columns with 0/1 options, any value different from '1' (including 0, NA, or an empty cell) will have the same effect as '0'."),
          
          tags$table(
            class = "table",
            tags$thead(
              tags$tr(
                tags$th("Field Name"),
                tags$th("Type"),
                tags$th("Description")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("field_name"), 
                tags$td("Text"), 
                tags$td("This should match the names of the column in your dataset.")
              ),
              tags$tr(
                tags$td("print"), 
                tags$td("0/1"), 
                tags$td("Indicates whether this field should be printed on the label (1 = YES).")
              ),
              tags$tr(
                tags$td("label_no"), 
                tags$td("Number (1-4)"), 
                tags$td("Fields can be printed on several thematic labels (e.g. date-location, ID...). Specify on which label (1 to 4) the field will be printed. You can print on up to 4 different labels.")
              ),
              tags$tr(
                tags$td("order_lab"), 
                tags$td("Number"), 
                tags$td("Set the order in which this field will appear on the label (1 = first position, 2 = second position...).")
              ),
              tags$tr(
                tags$td("print_field_name"), 
                tags$td("0/1"), 
                tags$td("Indicates whether you want to print an abbreviation or short name for this field (e.g., 'Loc.' for Location) (1 = YES).")
              ),
              tags$tr(
                tags$td("field_name_to_print"), 
                tags$td("Text"), 
                tags$td("If you want to print an abbreviation, enter it here. Some characters (eg. '_') may failed to be printed correctly")
              ),
              tags$tr(
                tags$td("print_opt_it"), 
                tags$td("0/1"), 
                tags$td("Indicates whether the text should be printed in italics (e.g. for genera and species names) (1 = YES).")
              ),
              tags$tr(
                tags$td("print_opt_par"), 
                tags$td("0/1"), 
                tags$td("Indicates whether the data should be printed inside parentheses (e.g. for subgenera names) (1 = YES).")
              ),
              tags$tr(
                tags$td("line_break"), 
                tags$td("0/1"), 
                tags$td("Indicates whether a line break should follow the data when printed (1 = YES). If the field is in the last position on a label, this will add a blank line below")
              ),
              tags$tr(
                tags$td("print_opt_hl"), 
                tags$td("0/1"), 
                tags$td("Indicates whether the text should be colored highlighted (1 = YES).")
              ),
              tags$tr(
                tags$td("print_sex_symbol"), 
                tags$td("0/1"), 
                tags$td("Indicates whether sex data should be printed as symbols (♂/♀) (1 = YES). Require that sex data begin with either 'f' or 'm' letters (not case-sensitive; for female and male, resp.). Only applicable for the sex field. ")
              )
            )
          ),
          
          h4("3. Adjust Label Layout and Appearance"),
          tags$p("You can customize the size and appearance of the labels:"),
          tags$ul(
            tags$li("Label Width (mm): Default is 15 mm."),
            tags$li("Label Height (mm): Default is 8 mm."),
            tags$li("Font Size: Default is 5."),
            tags$li("Number of Columns: Number of labels per row. Default is 8."),
            tags$li("Highlight Color: Set a color for highlighted fields (Default is 'orange', this should be an R named color).")
          ),
          
          h4("4. Generate the PDF"),
          tags$p("Once your data and parameters are configured:"),
          tags$ol(
            tags$li("Select the column for replication if you want multiple labels per entry."),
            tags$li("Click 'Download PDF' to generate and download the label sheet in .pdf format.")
          ),
          
          h4("5. Example Workflow"),
          tags$ol(
            tags$li("Upload your dataset (e.g., a .csv file with insect collection data)."),
            tags$li("Fill out or upload the printing parameters table."),
            tags$li("Adjust label size, font, columns, and highlight settings."),
            tags$li("Generate and download the PDF with the insect pinning labels.")
          ),
          
          h4("6. Troubleshooting"),
          tags$p("Ensure your dataset and parameters follow the specified structure. 
                 If the PDF generation fails, check for special characters in the field names that may cause LaTeX issues. 
                 If sex data are coded 'F'/'M' and datset only contains 'F's, this could be interpreted as 'FALSE' by R; consider recoding variable. 
                 Avoid using the degree symbol (°) e.g. for spatial coordinates, this may lead to printing issues. If you want the degree symbol anyway, a working 
                 hack consists in replacing the degree symbol in the data table by '\\smalldegree'.")
        ), 
        tabPanel(
          "Guide de l'utilisateur", 
          value = "panel5",
          h3("Guide de l'utilisateur : Générer des étiquettes pour l'épinglage d'insectes à l'aide de l'application Shiny"),
          tags$p("Ce guide vous aidera à utiliser l'application pour générer des étiquettes d'insectes à épingler à partir de votre ensemble de données et à configurer les paramètres d'impression des étiquettes."),
          
          h4("1. Téléchargez votre ensemble de données"),
          tags$p("Formats de fichier pris en charge : .csv (séparateur tabulation), .xlsx, .ods"),
          tags$ol(
            tags$li("Cliquez sur le bouton 'Browse...' et téléchargez le fichier contenant vos données d'insectes. Si c'est pour un test, vous pouvez utiliser un ensemble de données du package ETiquetteR (qui contient également des exemples de tableaux de paramètres d'impression, voir l'étape 2 ci-dessous) disponible ici : https://github.com/Nmoiroux/EtiquetteR/blob/main/inst/extdata/liste_ind_coll_ex.ods"),
            tags$li("Si votre fichier contient plusieurs feuilles, sélectionnez la feuille appropriée dans le menu déroulant. Vos données doivent apparaître dans l'onglet 'Data Table'")
          ),
          
          h4("2. Configurez le tableau des paramètres d'impression ('Print parameters table')"),
          tags$p("Le tableau des paramètres d'impression définit la manière dont les informations de votre ensemble de données seront affichées sur les étiquettes. 
                 Vous pouvez soit télécharger un tableau préconfiguré (stocké dans une feuille du fichier ods ou xlsx chargé ou dans un fichier csv) et le modifier éventuellement, soit remplir le tableau manuellement (selectionnez 'manually fill out' dans le menu déroulant). Voici un aperçu de chaque colonne du tableau et comment les configurer :"),
          tags$p(tags$strong("Remarque : "), "Pour les colonnes avec des options 0/1, toute valeur différente de '1' (y compris 0, NA ou une cellule vide) aura le même effet que '0'."),
          
          tags$table(
            class = "table",
            tags$thead(
              tags$tr(
                tags$th("Nom du champ"),
                tags$th("Type"),
                tags$th("Description")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Filed_name"), 
                tags$td("Texte"), 
                tags$td("Doit correspondre aux noms des colonnes de votre table de données.")
              ),
              tags$tr(
                tags$td("print"), 
                tags$td("0/1"), 
                tags$td("Indique si ce champ doit être imprimé sur l'étiquette (1 = OUI).")
              ),
              tags$tr(
                tags$td("label_no"), 
                tags$td("Nombre (1-4)"), 
                tags$td("Les champs peuvent être imprimés sur plusieurs étiquettes thématiques (par ex. date-lieu, ID...). Indiquez sur quelle étiquette (1 à 4) le champ sera imprimé. Vous pouvez imprimer jusqu'à 4 étiquettes différentes.")
              ),
              tags$tr(
                tags$td("order_lab"), 
                tags$td("Nombre"), 
                tags$td("Définissez l'ordre dans lequel ce champ apparaîtra sur l'étiquette (1 = première position, 2 = deuxième position...).")
              ),
              tags$tr(
                tags$td("print_field_name"), 
                tags$td("0/1"), 
                tags$td("Indique si vous souhaitez imprimer une abréviation ou un nom court pour ce champ (par ex., 'Loc.' pour Lieu) (1 = OUI).")
              ),
              tags$tr(
                tags$td("field_name_to_print"), 
                tags$td("Texte"), 
                tags$td("Si vous souhaitez imprimer une abréviation, saisissez-la ici. Certains caractères (par ex. '_') peuvent ne pas être imprimés correctement.")
              ),
              tags$tr(
                tags$td("print_opt_it"), 
                tags$td("0/1"), 
                tags$td("Indique si le texte doit être imprimé en italique (par ex. pour les noms de genres et d'espèces) (1 = OUI).")
              ),
              tags$tr(
                tags$td("print_opt_par"), 
                tags$td("0/1"), 
                tags$td("Indique si les données doivent être imprimées entre parenthèses (par ex. pour les noms de sous-genres) (1 = OUI).")
              ),
              tags$tr(
                tags$td("line_break"), 
                tags$td("0/1"), 
                tags$td("Indique si un saut de ligne doit suivre les données lorsqu'elles sont imprimées (1 = OUI). Si le champ est à la dernière position sur une étiquette, cela ajoutera une ligne vide en dessous.")
              ),
              tags$tr(
                tags$td("print_opt_hl"), 
                tags$td("0/1"), 
                tags$td("Indique si le texte doit être surligné (1 = OUI).")
              ),
              tags$tr(
                tags$td("print_sex_symbol"), 
                tags$td("0/1"), 
                tags$td("Indique si les données de sexe doivent être imprimées sous forme de symboles (♂/♀) (1 = OUI). Nécessite que les données de sexe commencent par les lettres 'f' ou 'm' (sans distinction de majuscules/minuscules; pour femelle et mâle, respectivement). Applicable uniquement pour le champ de sexe.")
              )
            )
          ),
          
          h4("3. Ajustez la disposition et l'apparence des étiquettes"),
          tags$p("Vous pouvez personnaliser la taille et l'apparence des étiquettes :"),
          tags$ul(
            tags$li("Label width (mm): Largeur de l'étiquette, par défaut 15 mm."),
            tags$li("Label height (mm): Hauteur de l'étiquette, par défaut 8 mm."),
            tags$li("Font size: Taille de police, par défaut 5."),
            tags$li("Number of column per page: Nombre d'étiquettes par ligne. Par défaut 8."),
            tags$li("Highlighting color: Couleur de surlignage, définissez une couleur pour les champs surlignés (par défaut 'orange', cela doit être un nom de couleur reconnu par R).")
          ),
          
          h4("4. Générez le PDF"),
          tags$p("Une fois vos données et paramètres configurés :"),
          tags$ol(
            tags$li("Sélectionnez la colonne pour la réplication si vous souhaitez plusieurs étiquettes par entrée."),
            tags$li("Cliquez sur 'Download PDF' pour générer et télécharger la feuille d'étiquettes au format .pdf.")
          ),
          
          h4("5. Exemple de flux de travail"),
          tags$ol(
            tags$li("Téléchargez votre ensemble de données (par ex., un fichier .csv avec des données de collection d'insectes)."),
            tags$li("Remplissez ou téléchargez le tableau des paramètres d'impression."),
            tags$li("Ajustez la taille des étiquettes, la police, les colonnes et les paramètres de surlignage."),
            tags$li("Générez et téléchargez le PDF avec les étiquettes pour le montage et l'épinglage des insectes.")
          ),
          
          h4("6. Dépannage"),
          tags$p("Assurez-vous que votre ensemble de données et vos paramètres respectent la structure spécifiée. 
         Si la génération du PDF échoue, vérifiez la présence de caractères spéciaux dans les noms de champs susceptibles de provoquer des problèmes avec LaTeX. 
         Si les données de sexe sont codées 'F'/'M' et que l'ensemble de données ne contient que des 'F', cela pourrait être interprété comme 'FALSE' par R ; envisagez de recoder la variable. 
         Évitez d'utiliser le symbole du degré (°) par ex. pour les coordonnées spatiales, cela pourrait entraîner des problèmes d'impression. Si vous souhaitez tout de même utiliser le symbole du degré, une solution consiste à remplacer le symbole du degré dans le tableau des données par '\\smalldegree'.")
        ),
        
        
        tabPanel("Data table", dataTableOutput('datatable'), value = "panel1"), # data table
        tabPanel( # printing parameters tables
          "Printing parameters table (editable)", dataTableOutput('print_info'), value = "panel2"
          #eDTOutput('print_info', width="100%", height = "100%")
        ), 
        tabPanel("About", value = "panel4", 
                 h3("About"),
                 tags$p("This shiny app is a friendly GUI for the EtiquetteR package : Moiroux, Nicolas, and Nil Rahola. ‘EtiquetteR: An R Package for Designing and Labeling Insects’, 2024. https://github.com/Nmoiroux/EtiquetteR .
"),
                 tags$p("The EtiquetteR package allow to generate a LaTeX document from a data table and a printing parameters table, and then compiling the LaTeX code 
                        into a PDF document. The LaTeX code is adapted from work by Samuel Brown (see https://github.com/sdjbrown/publicFiles/blob/master/labels.tex 
                                                                                                  and http://the-praise-of-insects.blogspot.com/2010/03/latex-insect-labels.html)."),
                 
                 h4("Aknowledgements"),
                 tags$p("")), 
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
    req(input$file1)
    file_ext <- tools::file_ext(input$file1$datapath)
    # Get sheet names depending on the file type
    sheets <- NULL
    if (file_ext == "xlsx") {
      sheets <- excel_sheets(input$file1$datapath)
      # case 1
    } else if (file_ext == "ods") {
      sheets <- list_ods_sheets(input$file1$datapath)  # Custom function to get sheet names
      #case  2
    } else if (file_ext == "csv") {
      output$q_upload_par_selector <- renderUI({
        selectInput("q_upload_par", "Select printing parameters table source (csv or manually-defined ?)", choices = c("manually fill out", "csv file"))
      })
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
        selectInput("sheet_par", "Select print parameters source", choices = c("choose...","manually fill out",sheets))
      })
    }
  })

  # should fileInput for print parameter file be enable ? (according to selectInput q_upload_par)
  observeEvent(input$q_upload_par,{
    req(input$q_upload_par)
    if(input$q_upload_par == "csv file"){
      output$file2_selector <- renderUI({
        fileInput("file2", "Upload your print parameters table (CSV)",
                  accept = c(".csv"))
      })
   
    } else if (input$q_upload_par == "user defined") {
      disable("file2_selector")
      #case  3 # to be used for print_information source of data
    }
  })
  
  observeEvent(input$file2,{
    req(input$file2)
    file_ext_2 <- tools::file_ext(input$file2$datapath)
    #case 4
  })
  
  observeEvent(input$sheet,{
    req(individuals()) # require data table exists (has been upload and sheet selected)
    
    output$datatable <- renderDT(individuals(),options = list( # display data table
      pageLength = 20)
      #, editable ="cell", selection = list(target = 'cell') # if we want the table to be editable
      )
    col_name <- colnames(individuals()) # retrieve column names for duplication number field
    output$field_N_selector <- renderUI({
      selectInput("col_N_name", "Select column for number of label replication", choices = c("no replication",col_name))
    })
  })
    
  # Reactive function to read user input dataset
  individuals <- reactive({
    req(input$file1, input$sheet)
    file_ext <- tools::file_ext(input$file1$datapath)
    if (input$sheet != "choose..."){ # if a file is selected
    if (file_ext == "csv") { # use the correct function to read the data table according to file extension (& selected sheet)
      read_csv(input$file1$datapath, delim = "\t") # require tab-delimited csv
    } else if (file_ext == "ods") {
      read_ods(input$file1$datapath, sheet = input$sheet)  # Use selected sheet
    } else if (file_ext == "xlsx") {
      read_xlsx(input$file1$datapath, sheet = input$sheet)  # Use selected sheet
    }}
  })

  
  # # Load the print_parameter table or template from EtiquetteR package
  print_informations <- reactive({
    
    col_name <- colnames(individuals())
    print_params <- NULL
    req(input$file1)
    f_ext <- tools::file_ext(input$file1$datapath) # retrieve file extension of the data table source
    if (f_ext == "xlsx" ){ #if data table is from an excel file
      req(input$sheet_par) # retrieve user defined sheet name for printing parameter
      if(!(input$sheet_par %in% c("choose...","manually fill out"))) { # user can select another sheet, if a sheet is selected
       print_params <- read_xlsx(input$file1$datapath, sheet = input$sheet_par) # read the table
       } else if (input$sheet_par == "manually fill out"){ # if user want to manually fill the table
         print_params <- empty_par(col_name) # retrieve only data table column names as row 
       }
      
      } else if (f_ext == "ods" ){ # if ods dataset
        req(input$sheet_par)
        if(!(input$sheet_par %in% c("choose...","manually fill out"))){
          print_params <- read_ods(input$file1$datapath, sheet = input$sheet_par)
        } else if (input$sheet_par == "manually fill out"){
          print_params <- empty_par(col_name)
        }
      } else if (f_ext == "csv"){ # if data table is from a csv file
        if(input$q_upload_par == "manually fill out"){ # user can manually fill printing parameters
          print_params <- empty_par(col_name)
        } else if (input$q_upload_par == "csv file"){ # or provide another tab-delimited csv file for printing parameters
          req(input$file2)
          print_params <- read_csv(input$file2$datapath, delim = "\t")
        }
      } else {
        print_params <- empty_par(col_name) # in all other case, an empty table to be manualy filled is provided
      }
    
    return(print_params)
  })
  

  # observe if printing information table has been generated and display
  observeEvent(input$sheet_par,{
    req(print_informations()) # verify that the reactive function has a result
    print_par$df <- print_informations()
   output$print_info <- renderDT(print_par$df, options = list( # display the editable table of printing parameters
     pageLength = 20), editable ="cell", selection = 'none')
   updateTabsetPanel( # update active panel
     session = getDefaultReactiveDomain(),
     "panel",
     selected = "panel2"
   )
  })
  
   # if Printing par table is edited
   observeEvent(input$print_info_cell_edit, {
     #print_par$df <<- editData(print_par$df, input$print_info_cell_edit, "print_info")
     print_par$df[input$print_info_cell_edit$row,input$print_info_cell_edit$col] <<- input$print_info_cell_edit$value
   })
     # modifiedData <- eDT(id = 'print_info', data = print_informations(),
     #                   options = list(dom = "Bfrtlip", keys = TRUE, ordering = FALSE, 
     #                                  autoFill = list(update = FALSE, focus = "focus"), 
     #                                  buttons = list("undo", "redo", "save"), pageLength = 20),
     #                   filter = "none")


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
        col_N_name = ifelse(input$col_N_name=="no replication",NA,input$col_N_name)
      )

      # Move the compiled PDF to the path requested by downloadHandler
      file.copy("labels.pdf", file, overwrite = TRUE) 
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
