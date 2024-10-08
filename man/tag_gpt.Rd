% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openai_api_call.R
\name{tag_gpt}
\alias{tag_gpt}
\title{Annotate Text Data Using OpenAI's GPT Models}
\usage{
tag_gpt(
  user_prompt,
  sys_prompt = "",
  temperature = 0,
  api_key = base::Sys.getenv("openai_key"),
  api_url = base::Sys.getenv("openai_url"),
  model = "gpt-3.5-turbo-0125",
  verbose = TRUE,
  storage = TRUE,
  rate_limit = 0.6
)
}
\arguments{
\item{user_prompt}{A character vector containing the text data to be annotated.}

\item{sys_prompt}{A character string containing the instruction for the GPT model to follow when annotating the text data. Default is an empty string.}

\item{temperature}{Numeric. The temperature parameter for the GPT model, controlling the randomness of the generated annotations. Default is 0.}

\item{api_key}{The API key for authenticating with the OpenAI API. By default, this is read from the "openai_key" environment variable.}

\item{api_url}{The URL for the OpenAI API endpoint. By default, this is read from the "openai_url" environment variable.}

\item{model}{The name of the GPT model to use for annotation. Default is "gpt-3.5-turbo-0125".}

\item{verbose}{Logical. If TRUE, prints progress and error messages. Default is TRUE.}

\item{storage}{Logical. If TRUE, saves the annotated responses as a CSV file. Default is TRUE.}

\item{rate_limit}{Numeric. The rate limit for API requests in seconds. Default is 0.6s.}
}
\value{
A character vector containing the annotated responses from the GPT API.
The function also saves the annotated responses as a CSV file in the "LLMoutput/columnname/model/.csv" path.
}
\description{
This function sends text data to OpenAI's GPT API for annotation based on a provided system prompt.
The annotated responses are saved as a CSV file.
}
\examples{
\dontrun{
my_data <- tibble(c("Apple", "Tomato", "Broccoli"))
system_prompt <- "Which one is a fruit?"
annotated_data <- tag_gpt(my_data, sys_prompt = system_prompt)
}
}
