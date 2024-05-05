#' Retrieve GPT responses for a column
#'
#' This function retrieves the GPT responses saved as RDS files for a given column. It
#' returns a tibble containing the "id" values from the original column and the corresponding
#' GPT-generated annotations.
#'
#' @param column The column for which to retrieve GPT responses. The name of this column
#' is used to locate the directory containing the saved RDS files.
#'
#' @param model The name of the GPT model used for annotation.
#'
#' @return A tibble with columns:
#' \describe{
#' \item{id}{The "id" values from the original column.}
#' \item{relevant_gpt}{The GPT-generated annotations, retrieved from the saved RDS files.}
#' }
#'
#' @examples
#' \dontrun{
#' my_data <- data.frame(
#' id = 1:3,
#' text = c("The cake is tasty", "The cake is terrible", "The cake is okay")
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
get_response <- function(column, model) {
  columnname <- base::deparse(base::substitute(column))
  filepath <- stringr::str_c("LLMoutput/", columnname, "/", model, "/")
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
    annotated_text = gpt_labels
  )
  return(gptoutput)
}


