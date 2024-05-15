#' Annotate a dataframe using OpenAI's GPT models
#'
#' This function sends the text data from a dataframe to OpenAI's GPT API for annotation,
#' based on a provided instruction. The GPT responses are saved as a CSV file.
#'
#' @param column A character vector containing the text data to be annotated.
#' @param instruction A character string containing the instruction for the GPT model to follow
#' when annotating the text data. Default is an empty string.
#' @param temperature Numeric. The temperature parameter for the GPT model, controlling the
#' randomness of the generated annotations. Default is 0.
#' @param api_key The API key for authenticating with the OpenAI API. By default, this is read
#' from the "openai_key" environment variable.
#' @param model The name of the GPT model to use for annotation. Default is "gpt-3.5-turbo-0125".
#' @param api_url The URL for the OpenAI API endpoint. By default, this is read from the
#' "openai_url" environment variable.
#' @return A data frame containing the annotated responses from the GPT API. The data frame has one column:
#' "aitag" (the annotated response from GPT).
#' The function also saves the annotated responses as a CSV file in the "LLMoutput/columnname/model/.csv" path.
#'
#' @examples
#' \dontrun{
#' my_data <- data.frame(
#'   id = c("1", "2", "3"),
#'   text = c("Example text 1", "Example text 2", "Example text 3")
#' )
#' annotated_data <- tag_gpt(my_data$id,
#' "Classify the sentiment of this text as positive, negative or neutral.")
#' }
#'
#' @importFrom httr2 request req_headers req_body_json req_timeout req_perform resp_body_json
#' @importFrom stringr str_c str_replace_all
#' @importFrom dplyr if_else
#' @importFrom utils write.csv
#' @export
tag_gpt <- function(column,
                    instruction = "",
                    temperature = 0,
                    api_key = Sys.getenv("openai_key"),
                    model = "gpt-3.5-turbo-0125",
                    api_url = Sys.getenv("openai_url")) {
  columnname <- base::deparse(base::substitute(column))
  filepath0 <- str_c("LLMoutput/", columnname, "/")
  if (!dir.exists(filepath0)) {
    dir.create(filepath0, recursive = TRUE)
  }
  filepath <- stringr::str_c(filepath0, model, ".csv")
  api_url <- stringr::str_c(api_url, "/v1/chat/completions")
  base::print(paste("Writing CSV file to:", filepath))

  result_df <- data.frame(id = character(), aitag = character(), stringsAsFactors = FALSE)

  for (i in 1:length(column)) {
    to_annotate_id <- column[i]
    to_annotate_text <- stringr::str_replace_all(column[i], "(\n|\r)", " ")
    authorization <- dplyr::if_else(api_url == "https://api.openai.com/v1/chat/completions", api_key, paste0("Bearer ", api_key))

    attempt <- 1
    max_attempts <- 3
    success <- FALSE

    while (!success && attempt <= max_attempts) {
      try({
        response <- request(api_url) |>
          req_headers(`Content-Type` = "application/json", `Authorization` = authorization) |>
          req_body_json(list(
            model = model,
            temperature = temperature,
            messages = list(
              list(role = "system", content = instruction),
              list(role = "user", content = to_annotate_text)
            )
          )) |>
          req_timeout(60000) |>
          req_perform()

        success <- TRUE
        response_content <- response |> httr2::resp_body_json()
        to_annotate_gptLabel <- response_content$choices[[1]]$message$content
        base::Sys.sleep(0.6)
        base::message(i, " of ", length(column))
        base::message("status_id: ", to_annotate_id, "\n", "instruction: ", instruction, "\n", "text: ", to_annotate_text)
        base::message("ChatGPT: ", to_annotate_gptLabel, "\n")
        result_df <- rbind(result_df, data.frame(aitag = to_annotate_gptLabel, stringsAsFactors = FALSE))
      }, silent = FALSE)
      if (!success) {
        base::Sys.sleep(2)
        attempt <- attempt + 1
      }
    }
    if (!success) {
      error_message <- paste0("Failed to fetch data after ", max_attempts, " attempts.")
      result_df <- rbind(result_df, data.frame(aitag = error_message, stringsAsFactors = FALSE))
    }
  }

  utils::write.csv(result_df, filepath, row.names = FALSE)
  return(result_df)
}
