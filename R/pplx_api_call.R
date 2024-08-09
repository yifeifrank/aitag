#' Annotate Text Data Using Perplexity's Models
#'
#' This function sends text data to Perplexity's API for annotation based on a provided system prompt.
#' The annotated responses are saved as a CSV file.
#'
#' @param user_prompt A character vector containing the text data to be annotated.
#' @param sys_prompt A character string containing the instruction for the Perplexity model to follow when annotating the text data. Default is "Be precise and concise.".
#' @param api_key The API key for authenticating with the Perplexity API. By default, this is read from the "perplexity_key" environment variable.
#' @param model The name of the Perplexity model to use for annotation. Default is "mistral-7b-instruct".
#' @param api_url The URL for the Perplexity API endpoint. Default is "https://api.perplexity.ai/chat/completions".
#' @param verbose Logical. If TRUE, prints progress and error messages. Default is TRUE.
#' @return A character vector containing the annotated responses from the Perplexity API.
#' The function also saves the annotated responses as a CSV file in the "LLMoutput/columnname/model.csv" path.
#' @examples
#' \dontrun{
#' my_data <- c("Apple", "Tomato", "Broccoli")
#' annotated_data <- tag_perplexity(my_data, sys_prompt = "Which one is a fruit?")
#' }
#' @importFrom httr2 request req_headers req_body_json req_timeout req_perform resp_body_json
#' @importFrom stringr str_c
#' @importFrom utils write.csv
#' @export
tag_perplexity <- function(user_prompt,
                           sys_prompt = "Be precise and concise.",
                           api_key = base::Sys.getenv("perplexity_key"),
                           model = "mistral-7b-instruct",
                           api_url = "https://api.perplexity.ai/chat/completions",
                           verbose = TRUE) {
  columnname <- base::deparse(base::substitute(user_prompt))
  api_key <- base::as.character(api_key)
  filepath0 <- stringr::str_c("LLMoutput/", columnname, "/")
  if (!base::dir.exists(filepath0)) {
    base::dir.create(filepath0, recursive = TRUE)
  }
  filepath <- stringr::str_c(filepath0, model, ".csv")
  if (verbose) base::print(base::paste("Writing CSV file to:", filepath))

  # Function to process each text entry
  annotate_text <- function(text, index, total) {
    to_annotate_text <- base::gsub("(\n|\r)", " ", text)
    authorization <- base::paste0("Bearer ", api_key)

    attempt <- 1
    max_attempts <- 3
    success <- FALSE
    result <- NA_character_

    while (!success && attempt <= max_attempts) {
      base::Sys.sleep(0.6) # Sleep to avoid rate limiting
      tryCatch({
        response <- httr2::request(api_url) |>
          httr2::req_headers(`Content-Type` = "application/json", `Authorization` = authorization) |>
          httr2::req_body_json(list(
            model = model,
            messages = list(
              list(role = "system", content = sys_prompt),
              list(role = "user", content = to_annotate_text)
            )
          )) |>
          httr2::req_timeout(60000) |>
          httr2::req_perform()

        if (response$status_code < 400) {
          response_content <- httr2::resp_body_json(response)
          result <- response_content$choices[[1]]$message$content
          success <- TRUE
          if (verbose) base::message("status_id: ", index, " of ", total, "\n", "sys_prompt: ", sys_prompt, "\n", "user_prompt: ", to_annotate_text)
          if (verbose) base::message("Perplexity: ", result, "\n")
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
  utils::write.csv(base::data.frame(response = annotations), filepath, row.names = FALSE)

  # Return the vector of annotations
  return(annotations)
}
