#' Annotate a dataframe using perplexity's models
#'
#' This function sends the text data from a dataframe to perplexity's API for annotation,
#' based on a provided instruction. The perplexity responses are saved as RDS files.
#'
#' @param column A character vector containing the data to be annotated.
#' @param instruction A character string containing the instruction for the perplexity model to follow
#' when annotating the text data.
#' @param api_key The API key for authenticating with the perplexity API. By default, this is read
#' from the "perplexity_key" environment variable.
#' @param model The name of the perplexity model to use for annotation. Default is "mistral-7b-instruct".
#' @param api_url The URL for the perplexity API endpoint. By default, this is set to
#' "https://api.perplexity.ai/chat/completions".
#' @return This function does not return a value. It saves the perplexity responses as RDS files in a
#' directory named "LLMoutput/{dataframe_name}/{model}.rds".
#'
#' @examples
#' \dontrun{
#' my_data <- data.frame(
#' id = 1:3,
#' text = c("Example text 1", "Example text 2", "Example text 3")
#' )
#' tag_perplexity(my_data$text, "Be precise and concise.")
#' }
#' @importFrom dplyr if_else
#' @importFrom stringr str_c
#' @importFrom httr POST add_headers content timeout
#' @export
tag_perplexity <- function(column,
                           instruction = "Be precise and concise.",
                           api_key = "",
                           model = "mistral-7b-instruct",
                           api_url = "https://api.perplexity.ai/chat/completions") {
  columnname <- base::deparse(base::substitute(column))
  api_key <- base::Sys.getenv("perplexity_key")
  api_key <- as.character(api_key)
  filepath <- stringr::str_c("LLMoutput/", columnname, "/",model,"/")
  if (!base::dir.exists(filepath)) {
    base::dir.create(filepath, recursive = TRUE)
  }
  base::print(base::paste("Writing RDS file to:", filepath))

  result_df <- base::data.frame(id = character(), response = character(), stringsAsFactors = FALSE)

  for (i in base::seq_along(column)) {
    to_annotate_id <- i
    to_annotate_text <- base::gsub("(\n|\r)", " ", column[i])
    authorization <- base::paste0("Bearer ", api_key)

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
            model = model,
            messages = list(
              list(role = "system", content = instruction),
              list(role = "user", content = to_annotate_text)
            )
          ),
          httr::timeout(10000)  # Correctly placed inside the POST function call
        )
        success <- TRUE
        response_content <- httr::content(response, "parsed")
        message_content <- response_content$choices[[1]]$message$content
        write_rds_path <- stringr::str_c(filepath, to_annotate_id, ".rds")
        base::saveRDS(response, write_rds_path)
        base::Sys.sleep(0.6)
        base::message(i, " of ", base::length(column))
        base::message("status_id: ", to_annotate_id, "\n", "instruction: ", instruction, "\n", "text: ", to_annotate_text)
        base::message("Perplexity: ", message_content, "\n")

        result_df <- base::rbind(result_df, base::data.frame(id = to_annotate_id, response = message_content, stringsAsFactors = FALSE))
      }, silent = FALSE)
      if (!success) {
        base::Sys.sleep(2)
        attempt <- attempt + 1
      }
    }
    if (!success) {
      result_df <- paste0("Failed to fetch data after ", max_attempts, " attempts.")
    }
  }
  return(result_df)
}
