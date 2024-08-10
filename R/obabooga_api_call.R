#' Annotate Text Data Using Oobabooga's Models
#'
#' This function sends text data to Oobabooga's API for annotation based on a provided system prompt.
#' The annotated responses are saved as a CSV file.
#'
#' @param user_prompt A character vector containing the text data to be annotated.
#' @param sys_prompt A character string containing the instruction for the Oobabooga model to follow when annotating the text data. Default is an empty string.
#' @param max_new_tokens The maximum number of tokens to generate in the response. Default is 250.
#' @param do_sample Whether to use sampling or not. If set to FALSE, the model will use greedy decoding. Default is TRUE.
#' @param temperature Numeric. The temperature parameter for the Oobabooga model, controlling the randomness of the generated annotations. Default is 1.
#' @param top_p Numeric. The top-p parameter for the Oobabooga model, controlling the cumulative probability threshold for token selection. Default is 0.9.
#' @param typical_p Numeric. The typical-p parameter for the Oobabooga model, controlling the typical sampling technique. Default is 1.
#' @param repetition_penalty Numeric. The repetition penalty parameter for the Oobabooga model, controlling the penalty for repeating tokens. Default is 1.05.
#' @param encoder_repetition_penalty Numeric. The encoder repetition penalty parameter for the Oobabooga model. Default is 1.
#' @param top_k Integer. The top-k parameter for the Oobabooga model, controlling the number of top tokens to consider for sampling. Default is 0.
#' @param min_length Integer. The minimum length of the generated response. Default is 0.
#' @param no_repeat_ngram_size Integer. The size of the n-grams to avoid repeating in the generated response. Default is 0.
#' @param num_beams Integer. The number of beams to use for beam search. Default is 1.
#' @param penalty_alpha Numeric. The length penalty parameter for the Oobabooga model. Default is 0.
#' @param length_penalty Numeric. The exponential length penalty parameter for the Oobabooga model. Default is 1.
#' @param early_stopping Whether to stop the generation when all beams reach the end of the sequence. Default is FALSE.
#' @param seed Integer. The random seed for reproducibility. Default is -1.
#' @param add_bos_token Whether to add the BOS (beginning of sequence) token to the generated response. Default is FALSE.
#' @param truncation_length Integer. The maximum length of the input sequence. Default is 2048.
#' @param ban_eos_token Whether to ban the EOS (end of sequence) token from being generated. Default is FALSE.
#' @param skip_special_tokens Whether to skip special tokens in the generated response. Default is FALSE.
#' @param api_url The URL for the Oobabooga API endpoint. Default is "http://localhost:5000/api/v1/generate".
#' @param verbose Logical. If TRUE, prints progress and error messages. Default is TRUE.
#' @param storage Logical. If TRUE, saves the annotated responses as a CSV file. Default is TRUE.
#' @param rate_limit Numeric. The rate limit for API requests in seconds. Default is 0s.
#' @return A character vector containing the annotated responses from the Oobabooga API.
#' The function also saves the annotated responses as a CSV file in the "LLMoutput/columnname/model.csv" path.
#' @examples
#' \dontrun{
#' my_data <- c("Apple", "Tomato", "Broccoli")
#' annotated_data <- tag_oobabooga(my_data, sys_prompt = "Which one is a fruit?")
#' }
#' @importFrom httr2 request req_headers req_body_json req_timeout req_perform resp_body_json
#' @importFrom stringr str_c
#' @importFrom utils write.csv
#' @export
tag_oobabooga <- function(user_prompt,
                          sys_prompt = "",
                          max_new_tokens = 250,
                          do_sample = TRUE,
                          temperature = 1,
                          top_p = 0.9,
                          typical_p = 1,
                          repetition_penalty = 1.05,
                          encoder_repetition_penalty = 1,
                          top_k = 0,
                          min_length = 0,
                          no_repeat_ngram_size = 0,
                          num_beams = 1,
                          penalty_alpha = 0,
                          length_penalty = 1,
                          early_stopping = FALSE,
                          seed = -1,
                          add_bos_token = FALSE,
                          truncation_length = 2048,
                          ban_eos_token = FALSE,
                          skip_special_tokens = FALSE,
                          api_url = "http://localhost:5000/api/v1/generate",
                          verbose = TRUE,
                          storage = TRUE,
                          rate_limit = 0) {
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
        response <- httr2::request(api_url) |>
          httr2::req_headers(`Content-Type` = "application/json") |>
          httr2::req_body_json(list(
            prompt = stringr::str_c(sys_prompt, "\n\n", to_annotate_text),
            max_new_tokens = max_new_tokens,
            do_sample = do_sample,
            temperature = temperature,
            top_p = top_p,
            typical_p = typical_p,
            repetition_penalty = repetition_penalty,
            encoder_repetition_penalty = encoder_repetition_penalty,
            top_k = top_k,
            min_length = min_length,
            no_repeat_ngram_size = no_repeat_ngram_size,
            num_beams = num_beams,
            penalty_alpha = penalty_alpha,
            length_penalty = length_penalty,
            early_stopping = early_stopping,
            seed = seed,
            add_bos_token = add_bos_token,
            truncation_length = truncation_length,
            ban_eos_token = ban_eos_token,
            skip_special_tokens = skip_special_tokens
          )) |>
          httr2::req_timeout(60000) |>
          httr2::req_perform()

        if (response$status_code < 400) {
          response_content <- httr2::resp_body_json(response)
          result <- response_content$results[[1]]$text
          success <- TRUE
          if (verbose) base::message("status_id: ", index, " of ", total, "\n", "sys_prompt: ", sys_prompt, "\n", "user_prompt: ", to_annotate_text)
          if (verbose) base::message("Oobabooga: ", result, "\n")
        } else {
          # Capture and return the error message from the response
          error_content <- httr2::resp_body_json(response)
          result <- error_content$detail
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

    filepath <- create_filepath(columnname,model="obabooga",formatted_time)
    write_csv(sys_prompt, user_prompt, annotations, current_time, filepath)
  }
  # Return the vector of annotations
  return(annotations)
}
