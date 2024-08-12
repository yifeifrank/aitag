#' Annotate Text Data Using OpenAI's GPT Models
#'
#' This function sends text data to OpenAI's GPT API for annotation based on a provided system prompt.
#' The annotated responses are saved as a CSV file.
#'
#' @param user_prompt A character vector containing the text data to be annotated.
#' @param sys_prompt A character string containing the instruction for the GPT model to follow when annotating the text data. Default is an empty string.
#' @param temperature Numeric. The temperature parameter for the GPT model, controlling the randomness of the generated annotations. Default is 0.
#' @param api_key The API key for authenticating with the OpenAI API. By default, this is read from the "openai_key" environment variable.
#' @param api_url The URL for the OpenAI API endpoint. By default, this is read from the "openai_url" environment variable.
#' @param model The name of the GPT model to use for annotation. Default is "gpt-3.5-turbo-0125".
#' @param verbose Logical. If TRUE, prints progress and error messages. Default is TRUE.
#' @param storage Logical. If TRUE, saves the annotated responses as a CSV file. Default is TRUE.
#' @param rate_limit Numeric. The rate limit for API requests in seconds. Default is 0.6s.
#' @return A character vector containing the annotated responses from the GPT API.
#' The function also saves the annotated responses as a CSV file in the "LLMoutput/columnname/model/.csv" path.
#' @examples
#' \dontrun{
#' my_data <- tibble(c("Apple", "Tomato", "Broccoli"))
#' system_prompt <- "Which one is a fruit?"
#' annotated_data <- tag_gpt(my_data, sys_prompt = system_prompt)
#' }
#' @importFrom httr2 request req_headers req_body_json req_timeout req_perform resp_body_json
#' @importFrom stringr str_c str_replace_all
#' @importFrom dplyr if_else
#' @importFrom utils write.csv
#' @export
tag_gpt <- function(user_prompt,
                    sys_prompt = "",
                    temperature = 0,
                    api_key = base::Sys.getenv("openai_key"),
                    api_url = base::Sys.getenv("openai_url"),
                    model = "gpt-3.5-turbo-0125",
                    verbose = TRUE,
                    storage = TRUE,
                    rate_limit = 0.6) {

  # Ensure the API URL is complete
  api_url <- stringr::str_c(api_url, "/v1/chat/completions")

  # Check if sys_prompt is a vector and handle accordingly
  if (is.vector(sys_prompt) && length(sys_prompt) == 1) {
    sys_prompt <- rep(sys_prompt, length(user_prompt))  # Repeat the single prompt
  } else if (is.vector(sys_prompt) && length(sys_prompt) != length(user_prompt)) {
    stop("Length of sys_prompt must be 1 or equal to the length of user_prompt.")
  }
  # Function to process each text entry
  annotate_text <- function(text, index, total) {
    to_annotate_text <- stringr::str_replace_all(text, "(\n|\r)", " ")
    authorization <- dplyr::if_else(api_url == "https://api.openai.com/v1/chat/completions", api_key, base::paste0("Bearer ", api_key))

    attempt <- 1
    max_attempts <- 3
    success <- FALSE
    result <- NA_character_

    while (!success && attempt <= max_attempts) {
      base::Sys.sleep(rate_limit) # Sleep to avoid rate limiting
      tryCatch({
        response <- httr2::request(api_url) |>
          httr2::req_headers(`Content-Type` = "application/json", `Authorization` = authorization) |>
          httr2::req_body_json(list(
            model = model,
            temperature = temperature,
            messages = list(
              list(role = "system", content = sys_prompt[index]),
              list(role = "user", content = to_annotate_text)
            )
          )) |>
          httr2::req_timeout(60000) |> # 60 seconds timeout
          httr2::req_perform()

        if (response$status_code < 400) {
          response_content <- httr2::resp_body_json(response)
          result <- response_content$choices[[1]]$message$content
          success <- TRUE
          if (verbose) {
            base::message("status_id: ", index, " of ", total, "\n", "sys_prompt: ", sys_prompt[index], "\n", "user_prompt: ", to_annotate_text)
            base::message("ChatGPT: ", result, "\n")
          }
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
  annotations <- purrr::map2_chr(user_prompt,
                                 base::seq_along(user_prompt),
                                 ~annotate_text(.x, .y, base::length(user_prompt)))

  if (storage) {
    current_time <- Sys.time()
    formatted_time <- format(current_time, "%Y-%m-%d_%H-%M-%S")
    columnname <- base::deparse(base::substitute(user_prompt))

    create_filepath <- function(columnname, model, formatted_time) {
      # Create the directory if it does not exist
      filepath0 <- stringr::str_c("llm_output/", columnname, "/")
      if (!base::dir.exists(filepath0)) {
        base::dir.create(filepath0, recursive = TRUE)
      }
      filepath <- stringr::str_c(filepath0, model, "_", formatted_time, ".csv")
      return(filepath)
    }

    write_csv <- function(sys_prompt, user_prompt, annotations, current_time, filepath) {
      # Print the filepath
      base::print(base::paste("Writing CSV file to:", filepath))
      # Write the CSV file
      utils::write.csv(data.frame(
        sys_prompt = sys_prompt,
        user_prompt = user_prompt,
        response = annotations,
        time = current_time
      ),
      filepath,
      row.names = FALSE)
    }

    filepath <- create_filepath(columnname, model, formatted_time)
    write_csv(sys_prompt, user_prompt, annotations, current_time, filepath)
  }

  # Return the vector of annotations
  return(annotations)
}
