# aitag

aitag is an R package that provides a convenient way to annotate text data using OpenAI's GPT models. It is designed to assist researchers and data analysts in extracting information from large numbers of political documents or any other text data at scale.

This package is based on the code from Haohan Chen's lecture on annotating political documents with ChatGPT, available at [https://github.com/haohanchen/Lecture_ChatGPT](https://github.com/haohanchen/Lecture_ChatGPT).

## Features

- Annotate text data using GPT models
- Save annotated files for future reference
- Monitor annotated texts in real-time while running
- Support for third-party GPT APIs
- Easy to resume from a break-point

## Installation

You can install the aitag package from GitHub using the `devtools` package:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install GPT-annotator from GitHub
devtools::install_github("Zeryfrank/aitag")
```

## Usage

To use the GPT-annotator package, you need to set up your OpenAI API key and URL as system environment variables:

```r
Sys.setenv(openai_api = "your_api_key")
Sys.setenv(openai_url = "https://api.openai.com/v1/chat/completions")
```

Then, load the package and use the `gpt_annotate` function to annotate your text data:

```r
library(GPT-annotator)

# Annotate data
gpt_annotate(your_dataframe, instruction = "Your instruction")
```

The annotated files will be saved in the `gptoutput/` directory, and you can monitor the progress and annotated texts in real-time.

To retrieve the GPT responses, use the `get_response` function:

```r
# Get responses
responses <- get_response(your_dataframe)
```

## Contributing

We welcome contributions to the GPT-annotator package. If you find a bug or have a feature request, please open an issue on the GitHub repository. If you would like to contribute code, please fork the repository and submit a pull request.

## License

The GPT-annotator package is released under the MIT License. See the `LICENSE` file for more information.

## Acknowledgments

We would like to thank Haohan Chen for providing the initial code and inspiration for this package through his lecture on annotating political documents with ChatGPT.

Citations:
[1] https://github.com/haohanchen/Lecture_ChatGPT

## License

This package is licensed under the [MIT License](LICENSE).
```
