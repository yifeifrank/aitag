# aitag

aitag is an R package that provides a convenient way to annotate text data using OpenAI's GPT models. It is designed to assist researchers and data analysts in extracting information from large numbers of political documents or any other text data at scale.


## Features

- Annotate text data using LLMs models
- Options for monitoring annotated texts in real-time while running
- Support for third-party LLM proxies
- Save annotated files for future reference
- Currently aitag supports three formats: openai(which is compatible with most LLM provider), claude, and perplexity (for online search)
  
## Installation

You can install the aitag package from GitHub using the `devtools` package:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install GPT-annotator from GitHub
devtools::install_github("yifeifrank/aitag")
```

## Usage

To use the GPT-annotator package, you need to set up your OpenAI API key and URL as system environment variables (no need to add v1/chat/completions in openai_url):

```r
Sys.setenv("openai_key" = "")
Sys.setenv("openai_url" = "")
```

Then, load the package and use the `aitag` function to annotate your text data:

```r
sentences <- data.frame(text = c("I love programming!", "This is so boring.", "The weather is nice today."))

# Define the system prompt for sentiment analysis
sys_prompt <- '<task>judge the sentiment of the following sentence</task>'

# Apply sentiment analysis using tag_gpt
sentences <- sentences %>%
  mutate(job_annotate = tag_gpt(text,
                                 sys_prompt = sys_prompt,
                                 model = 'gpt-4o-2024-05-13',
                                 rate_limit = 0.1))
# View the results
print(sentences)
```

## Contributing

We welcome contributions to the GPT-annotator package. If you find a bug or have a feature request, please open an issue on the GitHub repository. If you would like to contribute code, please fork the repository and submit a pull request.

## Acknowledgments
This package is inspired by the code from Haohan Chen's lecture on annotating political documents with ChatGPT, available at [https://github.com/haohanchen/Lecture_ChatGPT](https://github.com/haohanchen/Lecture_ChatGPT).
I would like to thank Haohan Chen for providing the initial code and inspiration for this package through his lecture on annotating political documents with ChatGPT.

## License

This package is licensed under the [MIT License](LICENSE).
```
