# gpt_annotator

`gpt_annotate` is an R package that provides functions to annotate data using OpenAI's GPT models. It serves as a basic infrastructure for GPT-assisted research and can be used as an alternative to writing complex regular expressions.

## Features

- Annotate data using GPT models
- Save annotated files for future reference
- Monitor annotated texts in real-time during the annotation process
- Support for third-party GPT APIs
- Easy resumption from break-points

## Installation

You can install the `gpt_annotate` package from GitHub using the `devtools` package:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install gpt_annotate from GitHub
devtools::install_github("your_username/gpt_annotate")
```

## Usage

Before using the package, make sure to set the following environment variables with your OpenAI API key and URL:

```r
Sys.setenv(openai_api = "your_api_key")
Sys.setenv(openai_url = "https://api.openai.com/v1/chat/completions")
```

Here's a basic example of how to use the `gpt_annotate` package:

```r
library(gpt_annotate)

# Annotate data
gpt_annotate(your_dataframe, instruction = "Your instruction")

# Get responses
responses <- get_response(your_dataframe)
```

The `gpt_annotate` function takes a dataframe and an instruction as input and annotates the data using GPT. The annotated files are saved for future reference.

The `get_response` function retrieves the GPT responses for the annotated data.

## Configuration

The `gpt_annotate` function has the following parameters:

- `dataframe`: The dataframe to annotate.
- `instruction`: The instruction for GPT.
- `temperature`: The temperature for GPT (default: 0.1).
- `api_key`: The OpenAI API key (default: Sys.getenv("openai_api")).
- `gpt_model`: The GPT model to use (default: "gpt-3.5-turbo-0125").
- `api_url`: The OpenAI API URL (default: Sys.getenv("openai_url")).

## Contributing

Contributions to the `gpt_annotate` package are welcome! If you encounter any issues or have suggestions for improvements, please open an issue or submit a pull request on the GitHub repository.

## License

This package is licensed under the [MIT License](LICENSE).
```
