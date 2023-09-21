library(arrow)
library(DT)

tsne_server <- function (input, output, session, session_info = NULL) {
  auth_token <- session$userData$auth0_credentials$access_token
  
  mydata <- reactive({
    # ---------- get cookie data from browser ----------
    cookie <- get_cookie(session_info$state)
    rid <- NULL
    if (!is.null(cookie)) {
      foundry_rids <- fromJSON(cookie) #parse as JSON, but if you wrote it in another format, parse as apt

      rid <- foundry_rids$inputRID
      output$display_query_params_box <- renderText(
        paste("Found cookie with inputRID : ", rid, " outputRID", foundry_rids$outputRID)
      )
  
    } else {
      print(paste("could not find cooke: ", session_info$state))
      output$display_query_params_box <- renderText(
        paste("ERROR: Could not find cookie with input dataset rid. State: ", session_info$state)
      )
      return(NULL)
    }
    # ---------- /get cookie data from browser ----------

    # ---------- read data from NIDAP to posit ----------
    nidap_list_file_url <- paste0("https://nidap.nih.gov/api/v1/datasets/",rid,"/files")
    response <- GET(nidap_list_file_url, httr::add_headers(Authorization = paste("Bearer", auth_token)))
    
    data_content <- content(response, as="text")
    parsed_json <- fromJSON(data_content)
    files <- parsed_json$data$path
    files <- files[!file_ext(files) %in% c("log", "")] #filter out log and spark success files

    # coalate files from dataset into R dataframe
    # I know Rui has a repo for this, this was made before I was aware of that, I will look into 
    df = data.frame()
    for (file in files) {
      file <- url_encode(file)
      get_file_content_url <- paste0("https://nidap.nih.gov/api/v1/datasets/",rid,"/files/",file,"/content")
      get_file_content_response <- GET(get_file_content_url, httr::add_headers(Authorization = paste("Bearer", auth_token)))
      
      if (file_ext(file) == "csv") {
        raw <- content(get_file_content_response, as="text")
        dataset <- read.csv(text = raw)
        dataset <- data.frame(dataset)
      } else if (file_ext(file) == "parquet") {
        raw <- content(get_file_content_response, as="raw")
        dataset <- arrow::read_parquet(raw)
        dataset <- data.frame(dataset)
        dataset$name <- file
      } 
      df <- rbind(df, dataset)
    }
    return(df)
  })

  observe({
    df <- mydata()
    if ( !is.null(df) ) {
      output$input_data <- DT::renderDataTable({df})
    }
  })

  observeEvent(
    input$upload, 
    {
      print("exporting to nidap")
      cookie <- cookies::get_cookie(session_info$state)

      if(!is.null(cookie)){
        cookie_json <- fromJSON(cookie)  
        rid <- cookie_json$outputRID      
        filePath <- sprintf("tempFile_from_posit-%s.csv", Sys.Date())
        
        data_to_upload <- mydata() # whatever var represents your transformed data
        data_to_upload <- head(data_to_upload)

        two_d_csv <- capture.output(write.csv(data_to_upload, row.names = FALSE)) #list of lists
        character_list <- paste(two_d_csv, collapse="\n")
        raw_char_array <- charToRaw(character_list)

        upload_url <- paste0("https://nidap.nih.gov/api/v1/datasets/",rid,"/files:upload?filePath=",filePath)

        response <- POST(
          upload_url, 
          content_type("application/octet-stream"),
          httr::add_headers(Authorization = paste("Bearer", auth_token)),
          body = raw_char_array
        )
        print(status_code(response))
        print(content(response))

      } else{
        output$upload_error_message_box <- renderText("ERROR, could not find upload RID in cookies")
      }
    }
  )
}
