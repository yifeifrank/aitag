#' Annotate a dataframe using OpenAI's GPT models
#'
#' This function sends the text data from a dataframe to OpenAI's GPT API for annotation,
#' based on a provided instruction. The GPT responses are saved as RDS files.
#' @param column A character vector containing the text data to be annotated.
#' @param instruction A character string containing the instruction for the GPT model to follow
#' when annotating the text data.
#' @param temperature Numeric. The temperature parameter for the GPT model, controlling the
#' randomness of the generated annotations. Default is 0.
#' @param api_key The API key for authenticating with the OpenAI API. By default, this is read
#' from the "openai_key" environment variable.
#' @param model The name of the GPT model to use for annotation. Default is "gpt-3.5-turbo-0125".
#' @param api_url The URL for the OpenAI API endpoint. By default, this is read from the
#' "openai_url" environment variable.
#' @return This function does not return a value. It saves the GPT responses as RDS files in a
#' directory named "LLMoutput/{columnname}/{model}.rds".
#' @examples
#' \dontrun{
#' my_data <- data.frame(
#' id = 1:3,
#' text = c("Example text 1", "Example text 2", "Example text 3")
#' )
#' gpt_annotate(my_data$text, "Classify the sentiment of this text as positive, negative or neutral.")
#' }
#' @importFrom dplyr if_else
#' @importFrom stringr str_c
#' @importFrom httr POST add_headers content timeout
#' @export
tag_gpt <- function(column,
                         instruction = "",
                         temperature = 0,
                         api_key = Sys.getenv("openai_key"),
                         model = "gpt-3.5-turbo-0125",
                         api_url = Sys.getenv("openai_url")) {
  columnname <- base::deparse(base::substitute(column))
  filepath <- stringr::str_c("LLMoutput/", columnname, "/",model,"/")
  api_url <- stringr::str_c(api_url, "/v1/chat/completions")
  if (!base::dir.exists(filepath)) {
    base::dir.create(filepath, recursive = TRUE)
  }
  base::print(paste("Writing RDS file to:", filepath))

  result_df <- data.frame(id = character(), aitag = character(), stringsAsFactors = FALSE)

  for (i in 1:length(column)) {
    to_annotate_id <- column[i]
    to_annotate_text <- stringr::str_replace_all(column[i], "(\n|\r)", " ")
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
            model = model,
            temperature = temperature,
            messages = list(
              list(role = "system", content = instruction),
              list(role = "user", content = to_annotate_text)
            )
          ),
          httr::timeout(10000)
        )
        success <- TRUE
        to_annotate_gptLabel <- httr::content(response)$choices[[1]]$message$content
        write_rds_path <- stringr::str_c(filepath, to_annotate_id, ".rds")
        base::saveRDS(response, write_rds_path)
        base::Sys.sleep(0.6)
        base::message(i, " of ", length(column))
        base::message("status_id: ", to_annotate_id, "\n", "instruction: ", instruction, "\n", "text: ", to_annotate_text)
        base::message("ChatGPT: ", to_annotate_gptLabel, "\n")

        # Append the result to the dataframe
        result_df <- rbind(result_df, data.frame(id = to_annotate_id, aitag = to_annotate_gptLabel, stringsAsFactors = FALSE))
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

