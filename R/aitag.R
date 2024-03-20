#' Annotate a dataframe using OpenAI's GPT models
#'
#' This function sends the text data from a dataframe to OpenAI's GPT API for annotation,
#' based on a provided instruction. The GPT responses are saved as RDS files.
#'
#' @param dataframe A dataframe containing the data to be annotated. It must include columns
#' named "id" and "text".
#' @param instruction A character string containing the instruction for the GPT model to follow
#' when annotating the text data.
#' @param temperature Numeric. The temperature parameter for the GPT model, controlling the
#' randomness of the generated annotations. Default is 0.
#' @param api_key The API key for authenticating with the OpenAI API. By default, this is read
#' from the "openai_key" environment variable.
#' @param gpt_model The name of the GPT model to use for annotation. Default is "gpt-3.5-turbo-0125".
#' @param api_url The URL for the OpenAI API endpoint. By default, this is read from the
#' "openai_url" environment variable.
#'
#' @return This function does not return a value. It saves the GPT responses as RDS files in a
#' directory named "gptoutput/{dataframe_name}/{id}.rds".
#'
#' @examples
#' \dontrun{
#' my_data <- data.frame(
#' id = 1:3,
#' text = c("Example text 1", "Example text 2", "Example text 3")
#' )
#'
#' gpt_annotate(my_data, "Classify the sentiment of this text as positive, negative or neutral.")
#' }
#'
#' @importFrom dplyr if_else
#' @importFrom stringr str_c 
#' @importFrom readxl read_excel
#' @importFrom httr POST add_headers content timeout
#' @importFrom utils read.table write.table
#' @export  
gpt_annotate <- function(dataframe, 
                         instruction = "", 
                         temperature = 0, 
                         api_key = Sys.getenv("openai_key"), 
                         gpt_model= "gpt-3.5-turbo-0125",
                         api_url = Sys.getenv("openai_url")) {
  dataframename <- base::deparse(base::substitute(dataframe))
  filepath <- stringr::str_c("gptoutput/", dataframename, "/")
  api_url <- stringr::str_c(api_url,"/v1/chat/completions")
  if (!base::dir.exists(filepath)) {
    base::dir.create(filepath, recursive = TRUE)
  }
  base::print(paste("Writing RDS file to:", filepath))
  for (i in 1:base::NROW(dataframe)) { 
    to_annotate_id <- dataframe$id[i]
    to_annotate_text <- stringr::str_replace_all(dataframe$text[i], "(\n|\r)", " ")
    authorization <- dplyr::if_else(api_url == "https://api.openai.com/v1/chat/completions", api_key, paste0("Bearer ", api_key))
    
    #retry times
    attempt <- 1
    max_attempts <- 3
    success <- FALSE
    
    while (!success && attempt <= max_attempts) {
      try({
        response <- httr::POST(
          url = api_url, 
          httr::add_headers(`Content-Type` = "application/json", `Authorization` = authorization),
          encode = "json",
          body = list(
            model = gpt_model,
            temperature = temperature,
            messages = list(
              list(role = "system", content = instruction),
              list(role = "user", content = to_annotate_text)
            )
          ),
          httr::timeout(5000)  # Correctly placed inside the POST function call
        )
        success <- TRUE
        to_annotate_gptLabel <- httr::content(response)$choices[[1]]$message$content
        write_rds_path <- stringr::str_c(filepath, to_annotate_id, ".rds")
        base::saveRDS(response, write_rds_path)
        base::Sys.sleep(0.6) # Be mindful of API rate limits
        base::message(i, " of ", base::nrow(dataframe))
        # Optional below: Print results to get a "live update"
        base::message("status_id: ", to_annotate_id, "\n","instruction: ",instruction, "\n" , "text: ", to_annotate_text)
        base::message("ChatGPT: ", to_annotate_gptLabel, "\n")
      }, silent = FALSE)
      if (!success) {
        base::Sys.sleep(2) # Wait for 2 seconds before retrying
        attempt <- attempt + 1
      }
    }
    if (!success) {
      base::stop("Failed to fetch data after ", max_attempts, " attempts.")
    }
  }
}
#' Retrieve GPT responses for a dataframe
#'
#' This function retrieves the GPT responses saved as RDS files for a given dataframe. It
#' returns a tibble containing the "id" values from the original dataframe and the corresponding
#' GPT-generated annotations.
#'
#' @param dataframe The dataframe for which to retrieve GPT responses. The name of this dataframe
#' is used to locate the directory containing the saved RDS files.
#'
#' @return A tibble with columns:
#' \describe{
#' \item{id}{The "id" values from the original dataframe.}
#' \item{relevant_gpt}{The GPT-generated annotations, retrieved from the saved RDS files.}
#' }
#'
#' @examples
#' \dontrun{
#' my_data <- data.frame(
#' id = 1:3,
#' text = c("Example text 1", "Example text 2", "Example text 3")
#' )
#'
#' gpt_annotate(my_data, "Classify the sentiment of this text as positive, negative or neutral.")
#'
#' annotations <- get_response(my_data)
#' }
#'
#' @importFrom purrr map_chr
#' @importFrom httr content
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @export              
get_response <- function(dataframe) {
  dataframename <- base::deparse(base::substitute(dataframe))
  filepath <- stringr::str_c("gptoutput/", dataframename, "/")
  file_names <- base::list.files(filepath, full.names = TRUE)
  gpt_labels <- base::rep(NA, base::length(file_names))
  for (i in base::seq_along(file_names)){
    response <- base::readRDS(file_names[i])
    gpt_labels[i] <- 
      ifelse(
        base::is.null(httr::content(response)$choices[[1]]$message$content),
        NA, httr::content(response)$choices[[1]]$message$content)
  }
  gptoutput <- tibble::tibble(
    id = stringr::str_replace(base::basename(file_names), "\\.rds$", ""),
    relevant_gpt = gpt_labels
  )
  return(gptoutput)
}

