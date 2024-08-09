#' Annotate Text Data Using Claude's Models
#'
#' This function sends text data to Claude's API for annotation based on a provided system prompt.
#' The annotated responses are saved as a CSV file.
#'
#' @param user_prompt A character vector containing the text data to be annotated.
#' @param sys_prompt A character string containing the instruction for the Claude model to follow when annotating the text data. Default is an empty string.
#' @param model The name of the Claude model to use for annotation. Default is "claude-v1".
#' @param max_tokens_to_sample The maximum number of tokens to generate in the response. Default is 100.
#' @param temperature Numeric. The temperature parameter for the Claude model, controlling the randomness of the generated annotations. Default is 1.
#' @param api_key The API key for authenticating with the Claude API. By default, this is read from the "CLAUDE_API_KEY" environment variable.
#' @param api_url The URL for the Claude API endpoint. Default is "https://api.anthropic.com".
#' @param verbose Logical. If TRUE, prints progress and error messages. Default is TRUE.
#' @param storage Logical. If TRUE, saves the annotated responses as a CSV file. Default is TRUE.
#' @param rate_limit Numeric. The rate limit for API requests in seconds. Default is 0.6s.
#' @return A character vector containing the annotated responses from the Claude API.
#' The function also saves the annotated responses as a CSV file in the "LLMoutput/columnname/model.csv" path.
#' @examples
#' \dontrun{
#' my_data <- c("Apple", "Tomato", "Broccoli")
#' annotated_data <- tag_claude(my_data, sys_prompt = "Which one is a fruit?")
#' }
#' @importFrom httr2 request req_headers req_body_json req_timeout req_perform resp_body_json
#' @importFrom stringr str_c
#' @importFrom utils write.csv
#' @export
tag_claude <- function(user_prompt,
                       sys_prompt = "",
                       model = "claude-v1",
                       max_tokens_to_sample = 100,
                       temperature = 1,
                       api_key = base::Sys.getenv("CLAUDE_API_KEY"),
                       api_url = "https://api.anthropic.com",
                       verbose = TRUE,
                       storage = TRUE,
                       rate_limit = 0.6) {
  columnname <- base::deparse(base::substitute(user_prompt))

  # Function to process each text entry
  annotate_text <- function(text, index, total) {
    to_annotate_text <- base::gsub("(\n|\r)", " ", text)

    attempt <- 1
    max_attempts <- 3
    success <- FALSE
    result <- NA_character_

    while (!success && attempt <= max_attempts) {
      base::Sys.sleep(rate_limit) # Sleep to avoid rate limiting
      tryCatch({
        response <- httr2::request(stringr::str_c(api_url, "/v1/complete")) |>
          httr2::req_headers(`Content-Type` = "application/json",
                             `x-api-key` = api_key) |>
          httr2::req_body_json(list(
            model = model,
            prompt = stringr::str_c(sys_prompt, "\n\n", to_annotate_text),
            max_tokens_to_sample = max_tokens_to_sample,
            temperature = temperature
          )) |>
          httr2::req_timeout(60000) |>
          httr2::req_perform()

        if (response$status_code < 400) {
          response_content <- httr2::resp_body_json(response)
          result <- response_content$completion
          success <- TRUE
          if (verbose) base::message("status_id: ", index, " of ", total, "\n", "sys_prompt: ", sys_prompt, "\n", "user_prompt: ", to_annotate_text)
          if (verbose) base::message("Claude: ", result, "\n")
        } else {
          # Capture and return the error message from the response
          error_content <- httr2::resp_body_json(response)
          result <- error_content$error$message
          if (verbose) base::message(base::paste("Error annotating text", index, ":", result))
          break
        }
      }, error = function(e) {
        if (verbose) base::message(base::paste("Error on text", index, ":", e$message))
      })

      if (!success) {
        attempt <- attempt + 1
        if (verbose) base::message(base::paste("Retrying text", index, "- Attempt", attempt, "of", max_attempts))
      }
    }

    if (!success && base::is.na(result)) {
      result <- "An unknown error occurred."
      if (verbose) base::message(base::paste("Failed to annotate text", index, "after", max_attempts, "attempts."))
    }

    return(result)
  }

  # Vectorize the function to process all entries in the column
  annotations <- purrr::map2_chr(user_prompt, base::seq_along(user_prompt), ~annotate_text(.x, .y, base::length(user_prompt)))

  # Write the results to a CSV file
  if (storage) filepath0 <- stringr::str_c("llm_output/", columnname, "/")
  if (storage) if (!base::dir.exists(filepath0)) {
    base::dir.create(filepath0, recursive = TRUE)
  }
  if (storage) filepath <- stringr::str_c(filepath0, model, ".csv")
  if (storage) base::print(base::paste("Writing CSV file to:", filepath))
  if (storage) utils::write.csv(data.frame(sys_prompt=sys_prompt,user_prompt=user_prompt,aitag = annotations), filepath, row.names = FALSE)

  # Return the vector of annotations
  return(annotations)
}
