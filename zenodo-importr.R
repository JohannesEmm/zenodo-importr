# Default Zenodo token
zenodo_token <- ""



setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to file directory

if(!require(pacman)) install.packages("pacman")
library(pacman)
p_load(shiny, shinydashboard, DT, httr2, jsonlite, RefManageR, R6, readxl, shinyjs)

# Null-default operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Zenodo BibTeX Importer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Step 1: Upload BibTeX File", status = "primary", solidHeader = TRUE, width = 12,
                    fileInput("bibtex_file", "Choose BibTeX File", accept = ".bib"),
                    helpText("Note: Only the first entry from the BibTeX file will be uploaded."),
                    conditionalPanel(
                      condition = "output.bib_loaded",
                      helpText("✓ Publication loaded and ready for upload."),
                      DT::dataTableOutput("bib_table")
                    )
                )
              ),
              fluidRow(
                box(title = "Step 2: Upload PDF File", status = "primary", solidHeader = TRUE, width = 12,
                    tags$head(tags$style(HTML("
                      .shiny-file-input-progress {display: none;}
                    "))),
                    fileInput("pdf_files", "Choose PDF File", 
                              accept = ".pdf", 
                              multiple = FALSE),
                    helpText("Upload a PDF file to attach to the publication."),
                    conditionalPanel(
                      condition = "output.pdf_loaded",
                      helpText("✓ PDF file loaded and ready for upload."),
                      DT::dataTableOutput("pdf_table")
                    )
                )
              ),
              fluidRow(
                box(title = "Step 3: Configure Upload", status = "primary", solidHeader = TRUE, width = 6,
                    fluidRow(
                      column(8,
                             textInput("token", "Zenodo Access Token", 
                                       value = zenodo_token,
                                       placeholder = "Enter your Zenodo token")
                      ),
                      column(4,
                             br(),
                             tags$a("Generate New Token", 
                                    href = "https://zenodo.org/account/settings/applications/tokens/new/", 
                                    target = "_blank", 
                                    class = "btn btn-info btn-sm")
                      )
                    ),
                    selectInput("access", "Access Type", choices = list("Open Access" = "open", "Closed Access" = "closed"), selected = "open"),
                    checkboxInput("auto_publish", "Auto-publish after upload", value = FALSE),
                    textAreaInput("ec_grants", "EC Grant Numbers (one per line, optional)", 
                                  placeholder = "e.g., 101022622"),
                    helpText("Enter European Commission grant numbers. These must exist in Zenodo's grants database."),
                    actionButton("upload_btn", "Upload to Zenodo", class = "btn-success")
                ),
                box(title = "Step 4: Results", status = "info", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("upload_results"),
                    conditionalPanel(
                      condition = "output.zenodo_url",
                      tags$p(tags$a("View My Uploads on Zenodo", 
                                    href = "https://zenodo.org/me/uploads?q=&f=shared_with_me%3Afalse&l=list&p=1&s=10&sort=newest", 
                                    target = "_blank",
                                    class = "btn btn-primary btn-sm"))
                    )
                )
              )
      ),
      tabItem(tabName = "settings",
              fluidRow(
                box(title = "Author ORCID Configuration", status = "primary", solidHeader = TRUE, width = 12,
                    p("Upload an Excel file with author names and ORCID IDs to automatically match them during upload."),
                    p("Excel file should have columns: 'Name' and 'ORCID'"),
                    fileInput("author_file", "Choose Excel File", accept = c(".xlsx", ".xls")),
                    conditionalPanel(
                      condition = "output.config_loaded",
                      DT::dataTableOutput("config_table")
                    )
                )
              )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Remove file size limit
  options(shiny.maxRequestSize = -1)
  
  values <- reactiveValues()
  
  # Check if BibTeX is loaded
  output$bib_loaded <- reactive({
    !is.null(values$bib_data)
  })
  outputOptions(output, "bib_loaded", suspendWhenHidden = FALSE)
  
  # Check if PDFs are loaded
  output$pdf_loaded <- reactive({
    !is.null(values$pdf_data)
  })
  outputOptions(output, "pdf_loaded", suspendWhenHidden = FALSE)
  
  # Check if config is loaded
  output$config_loaded <- reactive({
    !is.null(values$config)
  })
  outputOptions(output, "config_loaded", suspendWhenHidden = FALSE)
  
  # Zenodo API client - PRODUCTION ONLY
  ZenodoClient <- R6Class("ZenodoClient",
                          public = list(
                            token = NULL,
                            baseurl = NULL,
                            weburl = NULL,
                            
                            initialize = function(access_token) {
                              self$token <- access_token
                              self$baseurl <- "https://zenodo.org/api"
                              self$weburl <- "https://zenodo.org"
                              cat("DEBUG: ZenodoClient initialized - PRODUCTION MODE\n")
                            },
                            
                            upload_data = function(metadata, files = NULL, publish = FALSE) {
                              cat("DEBUG: Starting upload to Zenodo\n")
                              cat("DEBUG: Metadata title:", metadata$metadata$title, "\n")
                              
                              # Create deposition
                              url <- paste0(self$baseurl, "/deposit/depositions")
                              
                              tryCatch({
                                req <- request(url)
                                req <- req_headers(req, Authorization = paste("Bearer", self$token))
                                req <- req_headers(req, `Content-Type` = "application/json")
                                req <- req_body_json(req, metadata)
                                
                                resp <- req_perform(req)
                                status <- resp_status(resp)
                                
                                if (status != 201) {
                                  response_body <- resp_body_string(resp)
                                  if (status == 403) {
                                    stop("Access forbidden - check token permissions")
                                  } else {
                                    stop(paste("Upload failed with status", status, ":", response_body))
                                  }
                                }
                                
                                deposition <- resp_body_json(resp)
                                deposition_id <- deposition$id
                                bucket_url <- deposition$links$bucket
                                
                                cat("DEBUG: Deposition created with ID:", deposition_id, "\n")
                                cat("DEBUG: Bucket URL:", bucket_url, "\n")
                                
                                # Upload files if provided
                                upload_errors <- character()
                                if (!is.null(files) && length(files) > 0) {
                                  cat("DEBUG: === STARTING FILE UPLOADS ===\n")
                                  cat("DEBUG: Number of files to upload:", length(files), "\n")
                                  
                                  for (j in seq_along(files)) {
                                    file_info <- files[[j]]
                                    cat("DEBUG: Uploading file", j, "of", length(files), "\n")
                                    cat("DEBUG: File name:", file_info$name, "\n")
                                    cat("DEBUG: File path:", file_info$datapath, "\n")
                                    cat("DEBUG: File exists:", file.exists(file_info$datapath), "\n")
                                    if (file.exists(file_info$datapath)) {
                                      cat("DEBUG: File size:", file.info(file_info$datapath)$size, "bytes\n")
                                    }
                                    
                                    upload_success <- self$upload_file(bucket_url, file_info$datapath, file_info$name)
                                    if (!upload_success) {
                                      error_msg <- paste("File upload failed for:", file_info$name)
                                      cat("ERROR:", error_msg, "\n")
                                      upload_errors <- c(upload_errors, error_msg)
                                    } else {
                                      cat("SUCCESS: File uploaded successfully:", file_info$name, "\n")
                                    }
                                  }
                                  cat("DEBUG: === FILE UPLOADS COMPLETED ===\n")
                                  
                                  if (length(upload_errors) > 0) {
                                    cat("WARNING: Some files failed to upload:\n")
                                    for (err in upload_errors) {
                                      cat("  -", err, "\n")
                                    }
                                  }
                                } else {
                                  cat("DEBUG: No files to upload\n")
                                }
                                
                                # Publish if requested
                                if (publish) {
                                  self$publish_deposition(deposition_id)
                                }
                                
                                return(list(response = resp, deposition_id = deposition_id))
                                
                              }, error = function(e) {
                                stop(e$message)
                              })
                            },
                            
                            upload_file = function(bucket_url, file_path, file_name) {
                              cat("DEBUG: === FILE UPLOAD START ===\n")
                              cat("DEBUG: Bucket URL:", bucket_url, "\n")
                              cat("DEBUG: File path:", file_path, "\n")
                              cat("DEBUG: File name:", file_name, "\n")
                              
                              if (!file.exists(file_path)) {
                                cat("ERROR: File does not exist:", file_path, "\n")
                                return(FALSE)
                              }
                              
                              file_size <- file.info(file_path)$size
                              cat("DEBUG: File size:", file_size, "bytes\n")
                              
                              # Clean file name and URL encode if needed
                              clean_file_name <- gsub("[^A-Za-z0-9._-]", "_", file_name)
                              file_url <- paste0(bucket_url, "/", utils::URLencode(clean_file_name, reserved = TRUE))
                              cat("DEBUG: Clean file name:", clean_file_name, "\n")
                              cat("DEBUG: Upload URL:", file_url, "\n")
                              
                              tryCatch({
                                file_data <- readBin(file_path, "raw", n = file_size)
                                cat("DEBUG: Read", length(file_data), "bytes from file\n")
                                
                                req <- request(file_url)
                                req <- req_headers(req, Authorization = paste("Bearer", self$token))
                                req <- req_headers(req, `Content-Type` = "application/octet-stream")
                                req <- req_method(req, "PUT")
                                req <- req_body_raw(req, file_data)
                                
                                cat("DEBUG: Performing PUT request...\n")
                                resp <- req_perform(req)
                                status <- resp_status(resp)
                                
                                cat("DEBUG: Upload response status:", status, "\n")
                                
                                if (status == 200 || status == 201) {
                                  response_data <- tryCatch({
                                    resp_body_json(resp)
                                  }, error = function(e) NULL)
                                  
                                  if (!is.null(response_data)) {
                                    cat("DEBUG: ✓ File uploaded successfully! File ID:", response_data$id %||% "unknown", "\n")
                                  } else {
                                    cat("DEBUG: ✓ File uploaded successfully!\n")
                                  }
                                  return(TRUE)
                                } else {
                                  response_body <- tryCatch({
                                    resp_body_string(resp)
                                  }, error = function(e) "Could not read response")
                                  cat("DEBUG: ✗ File upload failed with status:", status, "\n")
                                  cat("DEBUG: Response body:", response_body, "\n")
                                  return(FALSE)
                                }
                                
                              }, error = function(e) {
                                cat("ERROR in file upload:", e$message, "\n")
                                return(FALSE)
                              })
                            },
                            
                            publish_deposition = function(deposition_id) {
                              publish_url <- paste0(self$baseurl, "/deposit/depositions/", deposition_id, "/actions/publish")
                              
                              req <- request(publish_url)
                              req <- req_headers(req, Authorization = paste("Bearer", self$token))
                              req <- req_method(req, "POST")
                              
                              resp <- req_perform(req)
                              return(resp_status(resp) == 202)
                            }
                          )
  )
  
  # Publication metadata processor
  PublicationProcessor <- R6Class("PublicationProcessor",
                                  public = list(
                                    process = function(bibentry, config = NULL, access_type = "open", grants = NULL) {
                                      bib_entry <- bibentry[[1]]
                                      
                                      # Initialize metadata
                                      metadata <- list(metadata = list())
                                      
                                      # Title
                                      title_val <- tryCatch(bib_entry$title, error = function(e) NULL)
                                      if (!is.null(title_val)) {
                                        metadata$metadata$title <- gsub("\\{|\\}", "", as.character(title_val))
                                      } else {
                                        metadata$metadata$title <- "Untitled Publication"
                                      }
                                      
                                      # Description
                                      abstract_val <- tryCatch(bib_entry$abstract, error = function(e) NULL)
                                      if (!is.null(abstract_val)) {
                                        metadata$metadata$description <- as.character(abstract_val)
                                      } else {
                                        metadata$metadata$description <- metadata$metadata$title
                                      }
                                      
                                      # Publication type
                                      metadata$metadata$upload_type <- "publication"
                                      metadata$metadata$publication_type <- "article"
                                      
                                      # Access rights
                                      metadata$metadata$access_right <- access_type
                                      
                                      # Authors
                                      author_list <- tryCatch(bib_entry$author, error = function(e) NULL)
                                      creators <- list()
                                      
                                      if (!is.null(author_list) && length(author_list) > 0) {
                                        for (i in seq_along(author_list)) {
                                          person_obj <- author_list[[i]]
                                          author_name <- tryCatch({
                                            as.character(person_obj, style = "text")
                                          }, error = function(e) "Unknown Author")
                                          
                                          if (length(author_name) > 0 && author_name != "" && author_name != "Unknown Author") {
                                            creator <- list(name = trimws(author_name))
                                            creators[[length(creators) + 1]] <- creator
                                          }
                                        }
                                      }
                                      
                                      if (length(creators) > 0) {
                                        metadata$metadata$creators <- creators
                                      } else {
                                        metadata$metadata$creators <- list(list(name = "Unknown Author"))
                                      }
                                      
                                      # Publication date
                                      year_val <- tryCatch(bib_entry$year, error = function(e) NULL)
                                      if (!is.null(year_val)) {
                                        year_num <- as.numeric(year_val)
                                        if (!is.na(year_num) && year_num > 1900 && year_num <= 2030) {
                                          metadata$metadata$publication_date <- paste0(year_num, "-01-01")
                                        } else {
                                          metadata$metadata$publication_date <- paste0(format(Sys.Date(), "%Y"), "-01-01")
                                        }
                                      } else {
                                        metadata$metadata$publication_date <- paste0(format(Sys.Date(), "%Y"), "-01-01")
                                      }
                                      
                                      # Journal fields
                                      journal_val <- tryCatch(bib_entry$journal, error = function(e) NULL)
                                      if (!is.null(journal_val)) {
                                        metadata$metadata$journal_title <- as.character(journal_val)
                                      }
                                      
                                      volume_val <- tryCatch(bib_entry$volume, error = function(e) NULL)
                                      if (!is.null(volume_val)) {
                                        metadata$metadata$journal_volume <- as.character(volume_val)
                                      }
                                      
                                      number_val <- tryCatch(bib_entry$number, error = function(e) NULL)
                                      if (!is.null(number_val)) {
                                        metadata$metadata$journal_issue <- as.character(number_val)
                                      }
                                      
                                      pages_val <- tryCatch(bib_entry$pages, error = function(e) NULL)
                                      if (!is.null(pages_val)) {
                                        metadata$metadata$journal_pages <- as.character(pages_val)
                                      }
                                      
                                      # DOI
                                      doi_val <- tryCatch(bib_entry$doi, error = function(e) NULL)
                                      if (!is.null(doi_val)) {
                                        metadata$metadata$doi <- as.character(doi_val)
                                      }
                                      
                                      # Language
                                      metadata$metadata$language <- "eng"
                                      
                                      # Grants
                                      if (!is.null(grants) && length(grants) > 0) {
                                        grants_list <- list()
                                        for (grant_id in grants) {
                                          grant_id_clean <- trimws(as.character(grant_id))
                                          if (nchar(grant_id_clean) > 0) {
                                            grants_list[[length(grants_list) + 1]] <- list(id = grant_id_clean)
                                          }
                                        }
                                        if (length(grants_list) > 0) {
                                          metadata$metadata$grants <- grants_list
                                        }
                                      }
                                      
                                      # License
                                      if (access_type == "open") {
                                        metadata$metadata$license <- "CC-BY-4.0"
                                      }
                                      
                                      return(metadata)
                                    }
                                  )
  )
  
  # BibTeX file upload handler - only process first entry
  observeEvent(input$bibtex_file, {
    req(input$bibtex_file)
    
    tryCatch({
      cat("DEBUG: Reading BibTeX file:", input$bibtex_file$name, "\n")
      
      bib <- ReadBib(input$bibtex_file$datapath)
      cat("DEBUG: Read", length(bib), "entries, will process only the first one\n")
      
      if (length(bib) == 0) {
        showNotification("No valid entries found in BibTeX file")
        return()
      }
      
      # Process only the first entry
      entry <- bib[1]
      bib_entry <- entry[[1]]
      
      # Extract key
      key_val <- tryCatch({
        entry_names <- names(entry)
        if (length(entry_names) > 0 && entry_names[1] != "") {
          entry_names[1]
        } else {
          "entry_1"
        }
      }, error = function(e) "entry_1")
      
      # Extract fields
      title_val <- tryCatch({
        title_raw <- bib_entry$title
        if (!is.null(title_raw) && length(title_raw) > 0) {
          as.character(title_raw)[1]
        } else {
          "..."
        }
      }, error = function(e) "...")
      
      year_val <- tryCatch({
        year_raw <- bib_entry$year
        if (!is.null(year_raw) && length(year_raw) > 0) {
          as.character(year_raw)[1]
        } else {
          ""
        }
      }, error = function(e) "")
      
      # Extract authors
      author_val <- tryCatch({
        author_list <- bib_entry$author
        if (!is.null(author_list) && length(author_list) > 0) {
          author_strings <- character()
          for (j in seq_along(author_list)) {
            person_obj <- author_list[[j]]
            author_name <- tryCatch({
              as.character(person_obj, style = "text")
            }, error = function(e) "Unknown")
            if (length(author_name) > 0 && author_name != "") {
              author_strings <- c(author_strings, author_name)
            }
          }
          if (length(author_strings) > 0) {
            paste(author_strings, collapse = " and ")
          } else {
            "..."
          }
        } else {
          "..."
        }
      }, error = function(e) "...")
      
      # Type
      type_val <- tryCatch({
        bibtype_raw <- attr(bib_entry, "bibtype")
        if (!is.null(bibtype_raw) && length(bibtype_raw) > 0) {
          tolower(as.character(bibtype_raw)[1])
        } else {
          "article"
        }
      }, error = function(e) "article")
      
      # Create single row data frame
      df <- data.frame(
        Key = as.character(key_val),
        Title = as.character(title_val), 
        Author = as.character(author_val),
        Year = as.character(year_val),
        Type = as.character(type_val),
        stringsAsFactors = FALSE
      )
      
      # Store data
      values$bib_data <- list(df = df, bib = entry)  # Store only first entry
      
      showNotification("Successfully loaded publication (first entry from BibTeX)")
      
    }, error = function(e) {
      showNotification(paste("Error reading BibTeX file:", e$message))
    })
  })
  
  # PDF file upload handler - simplified
  observeEvent(input$pdf_files, {
    req(input$pdf_files)
    
    tryCatch({
      cat("DEBUG: Processing", nrow(input$pdf_files), "PDF files\n")
      cat("DEBUG: Will use the first PDF for upload\n")
      
      # Create PDF mapping data frame
      pdf_df <- data.frame(
        Filename = input$pdf_files$name,
        Size_MB = round(input$pdf_files$size / (1024*1024), 2),
        Will_Upload = c("Yes", rep("No", nrow(input$pdf_files) - 1)),  # Only first one
        stringsAsFactors = FALSE
      )
      
      # Store the original file input for later use
      values$pdf_data <- list(df = pdf_df, files = input$pdf_files)
      
      showNotification(paste("Loaded", nrow(pdf_df), "PDF files. The first one will be uploaded."))
      
    }, error = function(e) {
      showNotification(paste("Error processing PDF files:", e$message))
    })
  })
  
  # Display tables
  output$bib_table <- DT::renderDataTable({
    req(values$bib_data)
    DT::datatable(values$bib_data$df, 
                  options = list(pageLength = 5, scrollX = TRUE, dom = 't'),
                  selection = 'none')
  })
  
  output$pdf_table <- DT::renderDataTable({
    req(values$pdf_data)
    DT::datatable(values$pdf_data$df, 
                  options = list(pageLength = 10, dom = 't'),
                  selection = 'none')
  })
  
  # Author configuration
  observeEvent(input$author_file, {
    req(input$author_file)
    
    tryCatch({
      author_data <- read_excel(input$author_file$datapath)
      
      if (!all(c("Name", "ORCID") %in% names(author_data))) {
        showNotification("Excel file must contain 'Name' and 'ORCID' columns")
        return()
      }
      
      authors_config <- apply(author_data, 1, function(row) {
        list(name = row["Name"], orcid = row["ORCID"])
      })
      
      values$config <- list(authors = authors_config)
      showNotification(paste("Loaded ORCID data for", length(authors_config), "authors"))
      
    }, error = function(e) {
      showNotification(paste("Error reading Excel file:", e$message))
    })
  })
  
  output$config_table <- DT::renderDataTable({
    req(values$config)
    
    config_df <- do.call(rbind, lapply(values$config$authors, function(author) {
      data.frame(Name = author$name, ORCID = author$orcid, stringsAsFactors = FALSE)
    }))
    
    DT::datatable(config_df, options = list(pageLength = 10))
  })
  
  # UPLOAD TO ZENODO - simplified for first entry + first PDF
  observeEvent(input$upload_btn, {
    cat("=== UPLOAD BUTTON CLICKED ===\n")
    
    # Validation
    if (is.null(values$bib_data)) {
      showNotification("Please upload a BibTeX file first")
      return()
    }
    
    token_val <- input$token
    if (is.null(token_val) || token_val == "") {
      showNotification("Please enter your Zenodo access token")
      return()
    }
    
    showNotification("Starting upload of publication with PDF")
    
    withProgress(message = "Uploading to Zenodo...", {
      
      # Process grants
      ec_grants <- NULL
      grants_input <- input$ec_grants
      if (!is.null(grants_input) && grants_input != "") {
        grants <- trimws(strsplit(grants_input, "\n")[[1]])
        grants <- grants[grants != ""]
        if (length(grants) > 0) {
          ec_grants <- grants
        }
      }
      
      # Configuration
      access_type <- input$access %||% "open"
      auto_publish <- input$auto_publish %||% FALSE
      
      # Initialize clients
      cat("DEBUG: Initializing Zenodo client\n")
      zenodo <- ZenodoClient$new(token_val)
      processor <- PublicationProcessor$new()
      
      # Always use the first (and only) publication entry
      bibentry <- values$bib_data$bib
      pub_key <- values$bib_data$df$Key[1]
      
      cat("DEBUG: === PROCESSING PUBLICATION ===\n")
      cat("DEBUG: Publication key:", pub_key, "\n")
      
      tryCatch({
        # Create metadata
        metadata <- processor$process(bibentry, values$config, access_type, ec_grants)
        
        # Prepare PDF files - use first PDF if available
        pdf_files <- NULL
        if (!is.null(values$pdf_data) && nrow(values$pdf_data$files) > 0) {
          cat("DEBUG: === PDF FILE PREPARATION ===\n")
          cat("DEBUG: Found", nrow(values$pdf_data$files), "PDF files, will use the first one\n")
          
          file_info <- list(
            name = values$pdf_data$files$name[1],
            datapath = values$pdf_data$files$datapath[1]
          )
          
          cat("DEBUG: First PDF file - name:", file_info$name, "\n")
          cat("DEBUG: First PDF file - path:", file_info$datapath, "\n")
          cat("DEBUG: File exists:", file.exists(file_info$datapath), "\n")
          
          if (file.exists(file_info$datapath)) {
            pdf_files <- list(file_info)
            cat("DEBUG: ✓ PDF file prepared for upload\n")
          } else {
            cat("DEBUG: ✗ PDF file does not exist\n")
          }
        } else {
          cat("DEBUG: No PDF files available\n")
        }
        
        # Upload
        cat("DEBUG: === STARTING ZENODO UPLOAD ===\n")
        cat("DEBUG: Will upload", if(is.null(pdf_files)) 0 else length(pdf_files), "PDF files\n")
        
        upload_result <- zenodo$upload_data(metadata, pdf_files, auto_publish)
        
        if (!is.null(upload_result$response) && resp_status(upload_result$response) == 201) {
          dep_id <- upload_result$deposition_id
          status_text <- if (auto_publish) "published" else "uploaded"
          pdf_text <- if (!is.null(pdf_files)) " with PDF" else ""
          result_msg <- paste("✓", pub_key, status_text, "successfully", pdf_text, "(ID:", dep_id, ")")
        } else {
          result_msg <- paste("✗", pub_key, "failed: unexpected response")
        }
        
        values$upload_results <- result_msg
        showNotification("Upload completed")
        
      }, error = function(e) {
        cat("ERROR:", e$message, "\n")
        values$upload_results <- paste("✗", pub_key, "failed:", e$message)
        showNotification("Upload failed")
      })
    })
  })
  
  # Output results
  output$upload_results <- renderText({
    values$upload_results %||% "No uploads yet"
  })
  
  # Always show the link button
  output$zenodo_url <- reactive({
    TRUE
  })
  outputOptions(output, "zenodo_url", suspendWhenHidden = FALSE)
}

# Run the app
shinyApp(ui = ui, server = server)