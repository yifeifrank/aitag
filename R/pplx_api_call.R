#' Annotate a dataframe using Perplexity's models
#'
#' This function sends the text data from a dataframe to Perplexity's API for annotation,
#' based on a provided instruction. The Perplexity responses are saved as a CSV file.
#'
#' @param column A character vector containing the data to be annotated.
#' @param instruction A character string containing the instruction for the Perplexity model to follow
#' when annotating the text data. Default is "Be precise and concise.".
#' @param api_key The API key for authenticating with the Perplexity API. By default, this is read
#' from the "perplexity_key" environment variable.
#' @param model The name of the Perplexity model to use for annotation. Default is "mistral-7b-instruct".
#' @param api_url The URL for the Perplexity API endpoint. Default is "https://api.perplexity.ai/chat/completions".
#' @return A data frame containing the annotated responses from the Perplexity API. The data frame has two columns:
#' "id" (the index of the input text) and "response" (the annotated response from Perplexity).
#' The function also saves the annotated responses as a CSV file in the "LLMoutput/dataframe_name/model.csv" path.
#'
#' @examples
#' \dontrun{
#' my_data <- data.frame(
#'   id = 1:3,
#'   text = c("Example text 1", "Example text 2", "Example text 3")
#' )
#' annotated_data <- tag_perplexity(my_data$text, "Be precise and concise.")
#' }
#'
#' @importFrom httr2 request req_headers req_body_json req_timeout req_perform resp_body_json
#' @importFrom stringr str_c
#' @importFrom utils write.csv
#' @export
tag_perplexity <- function(column,
                           instruction = "Be precise and concise.",
                           api_key = Sys.getenv("perplexity_key"),
                           model = "mistral-7b-instruct",
                           api_url = "https://api.perplexity.ai/chat/completions") {
  columnname <- base::deparse(base::substitute(column))
  api_key <- as.character(api_key)
  filepath0 <- str_c("LLMoutput/", columnname, "/")
  if (!dir.exists(filepath0)) {
    dir.create(filepath0, recursive = TRUE)
  }
  filepath <- stringr::str_c(filepath0, model, ".csv")
  base::print(base::paste("Writing CSV file to:", filepath))

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
        response <- request(api_url) |>
          req_headers(`Content-Type` = "application/json", `Authorization` = authorization) |>
          req_body_json(list(
            model = model,
            messages = list(
              list(role = "system", content = instruction),
              list(role = "user", content = to_annotate_text)
            )
          )) |>
          req_timeout(60000) |>
          req_perform()

        success <- TRUE
        response_content <- response |> httr2::resp_body_json()
        message_content <- response_content$choices[[1]]$message$content
        base::Sys.sleep(0.6)
        base::message(i, " of ", base::length(column))
        base::message("status_id: ", to_annotate_id, "\n", "instruction: ", instruction, "\n", "text: ", to_annotate_text)
        base::message("Perplexity: ", message_content, "\n")

        result_df <- base::rbind(result_df, base::data.frame(response = message_content, stringsAsFactors = FALSE))
      }, silent = FALSE)
      if (!success) {
        base::Sys.sleep(2)
        attempt <- attempt + 1
      }
    }
    if (!success) {
      error_message <- paste0("Failed to fetch data after ", max_attempts, " attempts.")
      result_df <- base::rbind(result_df, base::data.frame(response = error_message, stringsAsFactors = FALSE))
    }
  }

  utils::write.csv(result_df, filepath, row.names = FALSE)
  return(result_df)
}
